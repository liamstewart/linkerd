package io.l5d.experimental

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.param.Label
import com.twitter.finagle.{Http, Path}
import io.buoyant.consul.{CatalogNamer, SetHostFilter, v1}
import io.buoyant.linkerd.config.Parser
import io.buoyant.linkerd.config.types.Port
import io.buoyant.linkerd.{NamerConfig, NamerInitializer}

/**
 * Supports namer configurations in the form:
 *
 * <pre>
 * namers:
 * - kind: io.l5d.experimental.consul
 *   host: consul.site.biz
 *   port: 8600
 * </pre>
 */
class consul extends NamerInitializer {
  override def registerSubtypes(mapper: ObjectMapper): Unit =
    mapper.registerSubtypes(new NamedType(Parser.jClass[ConsulConfig], "io.l5d.consul"))
}

case class ConsulConfig(
  host: Option[String],
  port: Option[Port]
) extends NamerConfig {

  @JsonIgnore
  override def defaultPrefix: Path = Path.read("/io.l5d.consul")

  private[this] def getHost = host.getOrElse("localhost")
  private[this] def getPort = port match {
    case Some(p) => p.port
    case None => 8500
  }

  /**
   * Build a Namer backed by Consul.
   */
  @JsonIgnore
  def newNamer(): CatalogNamer = {
    val service = Http.client
      .configured(Label("namer" + prefix))
      .filtered(new SetHostFilter(getHost, getPort))
      .newService(s"/$$/inet/$getHost/$getPort")

    def mkNs(ns: String) = v1.Api(service)
    new CatalogNamer(prefix, mkNs)
  }
}

