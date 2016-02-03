package io.buoyant.linkerd.config

import io.buoyant.linkerd.config.validation.{ValidationUtils, Validated}
import io.buoyant.linkerd.config.validation.Validated._

trait LinkerConfig {
  def baseDtab: Option[String]
  def routers: Option[Seq[RouterConfig]]
  def namers: Option[Seq[NamerConfig]]
  def failFast: Option[Boolean]

  // Currently, the only thing we require of a Linker is that it has at least one Router configured.
  def validated: Validated[ConfigError, LinkerConfig.Validated] = {
    def validatedRouters = routers
      .filter { _.nonEmpty }
      .map(RouterConfig.validateRouters(this))
      .getOrElse(invalid[ConfigError, Seq[RouterConfig.Validated]](NoRoutersSpecified))
    def validatedNamers: Validated[ConfigError, Seq[NamerConfig.Validated]] = {
      val namerSeq: Seq[Validated[ConfigError, NamerConfig.Validated]] = namers getOrElse Seq.empty[NamerConfig] map { _.withDefaults(this).validated }
      ValidationUtils.transpose(namerSeq)
    }

    validatedRouters.ap(validatedNamers.map(namers => routers => new LinkerConfig.Validated(this, routers, namers)))
  }
}

object LinkerConfig {
  case class Impl(
    baseDtab: Option[String],
    failFast: Option[Boolean],
    namers: Option[Seq[NamerConfig]],
    routers: Option[Seq[RouterConfig]]
  ) extends LinkerConfig

  // Represents a fully validated LinkerConfig, with defaults applied, suitable for configuring a Linker.
  // NOTE: there are currently no defaults for Linkers, but probably will be in the future.
  class Validated(base: LinkerConfig, val routers: Seq[RouterConfig.Validated], val namers: Seq[NamerConfig.Validated]) {
    def baseDtab: Option[String] = base.baseDtab
  }
}
