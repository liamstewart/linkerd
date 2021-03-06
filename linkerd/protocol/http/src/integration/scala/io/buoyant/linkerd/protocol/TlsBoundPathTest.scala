package io.buoyant.linkerd
package protocol

import com.twitter.conversions.time._
import com.twitter.finagle.{Service, Failure}
import com.twitter.finagle.http.{Response, Request}
import io.buoyant.linkerd.protocol.TlsUtils._
import io.buoyant.test.Awaits
import io.l5d.clientTls.boundPath
import io.l5d.fs
import java.io.File
import org.scalatest.FunSuite
import scala.sys.process._

class TlsBoundPathTest extends FunSuite with Awaits {

  override val defaultWait = 2.seconds

  test("tls router + 2 tls backends") {

    withCerts("bill.buoyant.io", "ted.buoyant.io") { certs =>
      val bill = Downstream
        .constTls(
          "bill",
          "whoa",
          certs.serviceCerts("bill.buoyant.io").cert,
          certs.serviceCerts("bill.buoyant.io").key
        )
      val ted = Downstream
        .constTls(
          "ted",
          "dude",
          certs.serviceCerts("ted.buoyant.io").cert,
          certs.serviceCerts("ted.buoyant.io").key
        )
      try {
        withDisco(bill, ted) { disco =>

          val linkerConfig = s"""
             |namers:
             |- kind: io.l5d.fs
             |  rootDir: ${disco.getPath}
             |
             |routers:
             |- protocol: http
             |  baseDtab: |
             |    /http/1.1/GET => /srv ;
             |    /srv => /io.l5d.fs
             |
             |  servers:
             |  - port: 0
             |  client:
             |    tls:
             |      kind: io.l5d.clientTls.boundPath
             |      caCertPath: ${certs.caCert.getPath}
             |      names:
             |      - prefix: "/io.l5d.fs/{host}"
             |        commonNamePattern: "{host}.buoyant.io"
             |""".
              stripMargin
          withLinkerdClient(linkerConfig) { client =>
            val billRsp = {
              val req = Request()
              req.host = "bill"
              await(client(req))
            }
            assert(billRsp.contentString == "whoa")

            val tedRsp = {
              val req = Request()
              req.host = "ted"
              await(client(req))
            }
            assert(tedRsp.contentString == "dude")
          }
        }
      } finally (await(bill.server.close().join(ted.server.close())))
    }
  }

  test("mix of tls and plain backends") {

    withCerts("bill.buoyant.io") { certs =>
      val bill = Downstream
        .constTls(
          "bill",
          "whoa",
          certs.serviceCerts("bill.buoyant.io").cert,
          certs.serviceCerts("bill.buoyant.io").key
        )
      val ted = Downstream
        .const("ted", "dude")

      try {
        withDisco(bill, ted) { disco =>

          val linkerConfig = s"""
            |namers:
            |- kind: io.l5d.fs
            |  rootDir: ${disco.getPath}
            |
            |routers:
            |- protocol: http
            |  baseDtab: |
            |    /http/1.1/GET => /srv ;
            |    /srv => /io.l5d.fs
            |
            |  servers:
            |  - port: 0
            |  client:
            |    tls:
            |      kind: io.l5d.clientTls.boundPath
            |      caCertPath: ${certs.caCert.getPath}
            |      names:
            |      - prefix: "/io.l5d.fs/bill"
            |        commonNamePattern: "bill.buoyant.io"
            |""".
            stripMargin
          withLinkerdClient(linkerConfig) { client =>
            val billRsp = {
              val req = Request()
              req.host = "bill"
              await(client(req))
            }
            assert(billRsp.contentString == "whoa")

            val tedRsp = {
              val req = Request()
              req.host = "ted"
              await(client(req))
            }
            assert(tedRsp.contentString == "dude")
          }
        }
      } finally (await(bill.server.close().join(ted.server.close())))
    }
  }

  test("multiple name patterns") {

    withCerts("excellent", "righteous") { certs =>
      val bill = Downstream
        .constTls(
          "bill",
          "whoa",
          certs.serviceCerts("excellent").cert,
          certs.serviceCerts("excellent").key
        )
      val ted = Downstream
        .constTls(
          "ted",
          "dude",
          certs.serviceCerts("righteous").cert,
          certs.serviceCerts("righteous").key
        )

      try {
        withDisco(bill, ted) { disco =>

          val linkerConfig = s"""
            |namers:
            |- kind: io.l5d.fs
            |  rootDir: ${disco.getPath}
            |
            |routers:
            |- protocol: http
            |  baseDtab: |
            |    /http/1.1/GET => /srv ;
            |    /srv => /io.l5d.fs
            |
            |  servers:
            |  - port: 0
            |  client:
            |    tls:
            |      kind: io.l5d.clientTls.boundPath
            |      caCertPath: ${certs.caCert.getPath}
            |      names:
            |      - prefix: "/io.l5d.fs/bill"
            |        commonNamePattern: excellent
            |      - prefix: "/io.l5d.fs/ted"
            |        commonNamePattern: righteous
            |""".
            stripMargin
          withLinkerdClient(linkerConfig) { client =>
            val billRsp = {
              val req = Request()
              req.host = "bill"
              await(client(req))
            }
            assert(billRsp.contentString == "whoa")

            val tedRsp = {
              val req = Request()
              req.host = "ted"
              await(client(req))
            }
            assert(tedRsp.contentString == "dude")

          }
        }
      } finally (await(bill.server.close().join(ted.server.close())))
    }
  }

  test("wrong common name") {

    withCerts("bill.buoyant.io", "ted.buoyant.io") { certs =>
      val bill = Downstream
        .constTls(
          "bill",
          "whoa",
          certs.serviceCerts("ted.buoyant.io").cert,
          certs.serviceCerts("ted.buoyant.io").key
        )
      val ted = Downstream
        .constTls(
          "ted",
          "dude",
          certs.serviceCerts("bill.buoyant.io").cert,
          certs.serviceCerts("bill.buoyant.io").key
        )

      try {
        withDisco(bill, ted) { disco =>

          val linkerConfig = s"""
             |namers:
             |- kind: io.l5d.fs
             |  rootDir: ${disco.getPath}
             |
             |routers:
             |- protocol: http
             |  baseDtab: |
             |    /http/1.1/GET => /srv ;
             |    /srv => /io.l5d.fs
             |
             |  servers:
             |  - port: 0
             |  client:
             |    tls:
             |      kind: io.l5d.clientTls.boundPath
             |      caCertPath: ${certs.caCert.getPath}
             |      names:
             |      - prefix: "/io.l5d.fs/{host}"
             |        commonNamePattern: "{host}.buoyant.io"
             |""".
            stripMargin
          withLinkerdClient(linkerConfig) { client =>
            val billRsp = {
              val req = Request()
              req.host = "bill"
              intercept[Failure](await(client(req)))
            }

            val tedRsp = {
              val req = Request()
              req.host = "ted"
              intercept[Failure](await(client(req)))
            }
          }
        }
      } finally (await(bill.server.close().join(ted.server.close())))
    }
  }

  private[this] def withDisco(downstreams: Downstream*)(f: File => Unit): Unit = {
    val disco = new File("mktemp -d -t disco".!!.stripLineEnd)
    try {
      for (ds <- downstreams) {
        val w = new java.io.PrintWriter(new File(disco, ds.name))
        w.println(s"127.1 ${ds.port}")
        w.close()
      }
      f(disco)
    } finally (Seq("echo", "rm", "-rf", disco.getPath).!)
  }

  private[this] def withLinkerdClient(config: String)(f: Service[Request, Response] => Unit): Unit = {
    val namers = NamerInitializers(new fs)
    val protocols = ProtocolInitializers(new HttpInitializer)
    val tls = TlsClientInitializers(new boundPath)
    val linker = Linker.mk(protocols, namers, tls)
      .read(Yaml(config))
    val router = linker.routers.head.initialize()
    try {
      val server = router.servers.head.serve()
      try {
        val client = upstream(server)
        try {
          f(client)
        } finally (await(client.close()))
      } finally (await(server.close()))
    } finally (await(router.close()))
  }
}
