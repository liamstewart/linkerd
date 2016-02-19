package io.buoyant.linkerd

import com.fasterxml.jackson.annotation.{JsonTypeName, JsonSubTypes, JsonIgnore}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.{Addr, Name, NameTree, Namer, Path}
import com.twitter.util.{Activity, Var}
import io.buoyant.linkerd.config.Parser

class TestNamer extends NamerInitializer {
  /** Register config subtype */
  override def registerSubtypes(mapper: ObjectMapper): Unit =
    mapper.registerSubtypes(new NamedType(Parser.jClass[TestNamerConfig], "io.buoyant.linkerd.TestNamer"))
}

object TestNamer extends TestNamer

class TestNamerConfig extends NamerConfig { config =>
  @JsonIgnore
  override def defaultPrefix: Path = Path.read("/foo")

  var buh: Option[Boolean] = None

  @JsonIgnore
  override def newNamer(): Namer = new Namer {

    val buh = config.buh.getOrElse(false)

    def lookup(path: Path): Activity[NameTree[Name]] = {
      val t = path match {
        case Path.Utf8("buh", _*) if !buh => NameTree.Neg
        case path =>
          val addr = Var.value(Addr.Pending)
          NameTree.Leaf(Name.Bound(addr, prefix, path))
      }
      Activity.value(t)
    }
  }
}
