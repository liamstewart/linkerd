package io.buoyant.linkerd.config

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.twitter.finagle.Path
import io.buoyant.linkerd.config.validation.{Validated => CfgValidated}
import io.buoyant.linkerd.config.validation.Validated._

/**
 * Read a single namer configuration in the form:
 *
 * <pre>
 *   kind: io.l5d.izzle
 *   prefix: /i
 *   frizzle: dee
 *   swizzle: dah
 * </pre>
 *
 * In this example _io.l5d.izzle_ must be the _kind_ of a
 * [[NamerConfig]] in `namers`.  _frizzle_ and _swizzle_ are
 * namer-specific options.  This namer refines names beginning with
 * `/i` (after this prefix has been stripped).
 */
// TODO: switch to using class names once we have fully replaced the existing system.
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "kind")
trait NamerConfig {
  var prefix: Option[String] = None

  def withDefaults(linker: LinkerConfig): NamerConfig.Defaults =
    new NamerConfig.Defaults(this, protocol, linker)

  def protocol: NamerProtocol
}

trait NamerProtocol {
  def kind: String
  def validated: CfgValidated[ConfigError, NamerProtocol]
  def defaultPrefix: Option[String] = Some(Path.Utf8(kind).show)
}

object NamerConfig {
  class Defaults(base: NamerConfig, protocol: NamerProtocol, linker: LinkerConfig) {
    def prefix: Option[String] = base.prefix orElse protocol.defaultPrefix
    def validated: CfgValidated[ConfigError, NamerConfig.Validated] = {
      def validatedPrefix: CfgValidated[ConfigError, Path] = {
        prefix.fold(invalid[ConfigError, Path](MissingPath)) { pathStr =>
          try {
            valid(Path.read(pathStr))
          } catch {
            case ex: IllegalArgumentException => invalid(InvalidPath(pathStr, ex))
          }
        }
      }
      validatedPrefix.compose(protocol.validated) { (pfx, proto) => new Validated(pfx, proto) }
    }
  }

  class Validated(val prefix: Path, val protocol: NamerProtocol)
}
