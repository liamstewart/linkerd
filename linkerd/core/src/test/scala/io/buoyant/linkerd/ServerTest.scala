package io.buoyant.linkerd

import com.twitter.finagle.transport.Transport
import com.twitter.util.{Return, Try}
import java.net.{InetAddress, InetSocketAddress}
import org.scalatest.FunSuite

class ServerTest extends FunSuite {

  def parse(proto: ProtocolInitializer, yaml: String): Try[Server] =
    Try(proto.server.configuredFrom(Yaml(yaml)))

  val plainYaml = """
port: 1234
"""

  val fancyYaml = """
port: 1234
fancyServer: true
"""

  test("addr parsed") {
    val Return(server) = parse(TestProtocol.Plain, """
port: 4321
ip: 8.8.8.8
""")
    assert(server.addr == new InetSocketAddress("8.8.8.8", 4321))
  }

  test("addr no ip is loopback") {
    val Return(server) = parse(TestProtocol.Plain, """
port: 4320
""")
    assert(server.addr == new InetSocketAddress(InetAddress.getLoopbackAddress, 4320))
  }

  test("addr any ip") {
    val Return(server) = parse(TestProtocol.Plain, """
port: 432
ip: 0.0.0.0
""")
    assert(server.addr == new InetSocketAddress(432))
  }

  test("no port") {
    val yaml = """
ip: 127.1
"""
    assert(parse(TestProtocol.Plain, yaml).isThrow)
  }

  test("unknown server params error") {
    assert(parse(TestProtocol.Plain, fancyYaml).isThrow)
  }

  test("protocol-specific server defaults") {
    val Return(server) = parse(TestProtocol.Fancy, plainYaml)
    assert(!server.params[TestProtocol.Fancy.Pants].fancy)
  }

  test("protocol-specific server params") {
    val Return(server) = parse(TestProtocol.Fancy, fancyYaml)
    assert(server.params[TestProtocol.Fancy.Pants].fancy)
  }

  test("protocol-specific router params have no bearing on servers") {
    val yaml = """
port: 1234
fancyRouter: true
"""
    assert(parse(TestProtocol.Fancy, yaml).isThrow)
  }

  test("invalid tls configuration") {
    val yaml =
      """
        |port: 1234
        |tls:
      """.stripMargin
    assert(parse(TestProtocol.Plain, yaml).isThrow)
  }

  test("valid tls configuration") {
    val yaml =
      """
        |port: 1234
        |tls:
        |  certPath: /foo/cert
        |  keyPath: /foo/key
      """.stripMargin
    assert(parse(TestProtocol.Plain, yaml).get.params.apply[Transport.TLSServerEngine].e.isDefined)
  }

  test("tls configuration absent") {
    val yaml =
      """
        |port: 1234
      """.stripMargin
    assert(parse(TestProtocol.Plain, yaml).get.params.apply[Transport.TLSServerEngine].e.isEmpty)
  }
}
