package io.buoyant.linkerd

import com.fasterxml.jackson.core.{JsonParser, JsonToken, TreeNode}
import com.twitter.finagle.Stack
import com.twitter.finagle.buoyant.DstBindingFactory

/**
 * Represents the total configuration of a Linkerd process.
 *
 * `params` are default router params defined in the top-level of a
 * linker configuration, and are used when reading [[Router Routers]].
 */
trait Linker {
  def protocols: ProtocolInitializers
  def tls: TlsClientInitializers

  def params: Stack.Params
  def withParams(ps: Stack.Params): Linker
  def configured[P: Stack.Param](p: P): Linker

  def routers: Seq[Router]
  def withRouters(routers: Seq[Router]): Linker
  def routing(r: Router): Linker

  def admin: Admin
  def withAdmin(a: Admin): Linker

  /**
   * Read a [[Linker]] from the given parser, using the provided
   * protocol support.
   *
   * A linker configuration consists of the following:
   *  - arbitrary default [[Router.Params router params]]
   *  - a required `routers` field with a list of [[Router]] specs
   *
   * Example configuration:
   *
   * <pre>
   *    admin:
   *      port: 9991
   *
   *     baseDtab: |
   *       /bar => /bah;
   *       /http/1.1 => /b;
   *
   *     routers:
   *      - protocol: http
   *        servers:
   *        - port: 4140
   *        httpUriInDst: true
   *
   *      - protocol: http
   *        label: http.ext
   *        dstPrefix: /ext/http
   *        servers:
   *        - port: 8080
   *          ip: any
   *
   *      - protocol: thrift
   *        client:
   *          thriftFramed: true
   *        servers:
   *        - port: 9090
   *
   * </pre>
   *
   * Note that protocol-specific features are delegated to per-
   * protocol [[ProtocolInitializer]] plugins.
   *
   * Ordering of fields is irrelevant.
   */
  def read(p: JsonParser): Linker
}

object Linker {

  def mk(protos: ProtocolInitializers, namers: NamerInitializers, tls: TlsClientInitializers): Linker =
    Impl(protos, namers, tls, Stack.Params.empty, Nil, Admin())

  def load(): Linker = {
    val protos = ProtocolInitializers.load()
    val namers = NamerInitializers.load()
    val tls = TlsClientInitializers.load()
    mk(protos, namers, tls)
  }

  /**
   * Private concrete implementation, to help protect compatibility if
   * the Linker api is extended.
   */
  private case class Impl(
    protocols: ProtocolInitializers,
    namers: NamerInitializers,
    tls: TlsClientInitializers,
    params: Stack.Params,
    routers: Seq[Router],
    admin: Admin
  ) extends Linker {
    def withParams(ps: Stack.Params): Linker = copy(params = ps)
    def configured[P: Stack.Param](p: P): Linker = withParams(params + p)

    def withRouters(rs: Seq[Router]): Linker = copy(routers = rs)
    def routing(r: Router): Linker = withRouters(routers :+ r)

    def withAdmin(a: Admin): Linker = copy(admin = a)

    def read(json: JsonParser): Linker = {
      // Make a pass through the linker structure, deferring processing
      // of the `routers` list until after all other fields (params)
      // have been processed.
      val (linkerWithParams, routers) = {
        val init: (Linker, Option[TreeNode]) = (this, None)
        Parsing.foldObject(json, init) {
          case ((linker, routers), "routers", json) =>
            Parsing.ensureTok(json, JsonToken.START_ARRAY) { json =>
              val tree: TreeNode = json.readValueAsTree()
              json.nextToken()
              (linker, Some(tree))
            }

          case ((linker, rs), "namers", json) =>
            // In theory, this could be treated as a generic
            // Parsing.Params, however since it requires `namers`, it's
            // easier just to read this in-line here.
            val namer = namers.read(json)
            val l = linker.configured(DstBindingFactory.Namer(namer))
            (l, rs)

          case ((linker, rs), name, json) if Router.Params.parser.keys(name) =>
            val params = Router.Params.parser.read(name, json, linker.params)
            val l = linker.withParams(params)
            (l, rs)

          case ((linker, rs), "admin", json) =>
            Parsing.ensureTok(json, JsonToken.START_OBJECT) { json =>
              val l = withAdmin(admin.read(json))
              (l, rs)
            }

          case (_, name, json) => throw Parsing.error(s"unknown parameter: $name", json)
        }
      }

      // Parse 'routers' with linker-level params as defaults.
      val tree = routers.getOrElse {
        throw Parsing.error("'routers' not specified", json)
      }
      val tp = tree.traverse()
      tp.setCodec(json.getCodec)
      tp.nextToken()
      tp.overrideCurrentName("routers")

      // Collect routers from config, ensuring that there are no
      // name conflicts and that all servers are configured to bind
      // on distinct sockets.
      val linkerWithRouters = Parsing.foldArray(tp, linkerWithParams) {
        case (linker, json) =>
          val router = Router.read(json, linker.params, linker.protocols, linker.tls)

          // Ensure that neither the router name nor server sockets conflict.
          for (other <- linker.routers) {
            if (router.label == other.label)
              throw Parsing.error(s"conflicting routers named '${router.label}'", json)
            for ((s0, s1) <- Server.findConflict(router.servers ++ other.servers))
              throw Parsing.error(s"conflicting servers: ${s0.addr}, ${s1.addr}", json)
          }

          linker.routing(router)
      }
      if (linkerWithRouters.routers.isEmpty)
        throw Parsing.error("linker must have at least one router", json)
      linkerWithRouters
    }
  }
}
