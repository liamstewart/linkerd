package io.buoyant.linkerd

import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.naming.NameInterpreter
import com.twitter.finagle.{Dtab, Name, NameTree, Path}
import io.buoyant.linkerd.config.Parser
import org.scalatest.FunSuite

class booNamer extends TestNamerConfig {
  override def defaultPrefix = Path.read("/boo")
}

class booUrnsNamer extends TestNamerConfig {
  override def defaultPrefix = Path.read("/boo/urns")
}

class NamerInitializersTest extends FunSuite {

  def interpreter(config: String): NameInterpreter = {
    val mapper = Parser.objectMapper(config)
    mapper.registerSubtypes(new NamedType(Parser.jClass[booNamer], "io.buoyant.linkerd.booNamer"))
    mapper.registerSubtypes(new NamedType(Parser.jClass[booUrnsNamer], "io.buoyant.linkerd.booUrnsNamer"))
    val cfg = mapper.readValue[Seq[NamerConfig]](config)
    Linker.nameInterpreter(cfg)
  }

  test("namers evaluated bottom-up") {
    val path = Path.read("/boo/urns")

    val booYaml =
      """|- kind: io.buoyant.linkerd.booUrnsNamer
         |- kind: io.buoyant.linkerd.booNamer
         |""".stripMargin
    interpreter(booYaml).bind(Dtab.empty, path).sample() match {
      case NameTree.Leaf(bound: Name.Bound) =>
        assert(bound.id == Path.read("/boo"))
        assert(bound.path == Path.read("/urns"))
      case tree => fail(s"unexpected result: $tree")
    }

    val booUrnsYaml =
      """|- kind: io.buoyant.linkerd.booNamer
         |- kind: io.buoyant.linkerd.booUrnsNamer
         |""".stripMargin

    interpreter(booUrnsYaml).bind(Dtab.empty, path).sample() match {
      case NameTree.Leaf(bound: Name.Bound) =>
        assert(bound.id == Path.read("/boo/urns"))
        assert(bound.path == Path.empty)
      case tree => fail(s"unexpected result: $tree")
    }
  }
}
