package io.l5d.experimental

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.conversions.time._
import com.twitter.finagle.param.Label
import com.twitter.finagle.{Http, Path}
import io.buoyant.linkerd.config.Parser
import io.buoyant.linkerd.config.types.Port
import io.buoyant.linkerd.{NamerConfig, NamerInitializer}
import io.buoyant.marathon.v2.{Api, AppIdNamer}

/**
 * Supports namer configurations in the form:
 *
 * <pre>
 * namers:
 * - kind:      io.l5d.experimental.marathon
 *   prefix:    /io.l5d.marathon
 *   host:      marathon.mesos
 *   port:      80
 *   uriPrefix: /marathon
 * </pre>
 */
class marathon extends NamerInitializer {
  override def registerSubtypes(mapper: ObjectMapper): Unit =
    mapper.registerSubtypes(new NamedType(Parser.jClass[MarathonConfig], "io.l5d.experimental.marathon"))
}

case class MarathonConfig(
  host: Option[String],
  port: Option[Port],
  uriPrefix: Option[String]
) extends NamerConfig {
  @JsonIgnore
  override def defaultPrefix: Path = Path.read("/io.l5d.marathon")

  private[this] def getHost = host.getOrElse("marathon.mesos")
  private[this] def getPort = port match {
    case Some(p) => p.port
    case None => 80
  }
  private[this] def getUriPrefix = uriPrefix.getOrElse("")

  /**
   * Construct a namer.
   */
  def newNamer() = {
    val service = Http.client
      .configured(Label("namer" + prefix.show))
      .newService(s"/$$/inet/$getHost/$getPort")

    new AppIdNamer(Api(service, getHost, getUriPrefix), prefix, 250.millis)
  }
}
