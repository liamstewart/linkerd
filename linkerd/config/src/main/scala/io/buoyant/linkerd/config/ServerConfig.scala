package io.buoyant.linkerd.config

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.net.InetAddresses
import io.buoyant.linkerd.config.validation.{Validated => CfgValidated, ValidationUtils}
import io.buoyant.linkerd.config.validation.Validated._

trait ServerConfig {
  def ip: Option[String]
  def port: Option[Int]

  // TODO: unify this code with what's in Server
  private[this] val loopbackIp = InetAddress.getLoopbackAddress
  private[this] val anyIp = InetAddress.getByAddress(Array(0, 0, 0, 0))
  def addr: InetSocketAddress = new InetSocketAddress(
    ip map InetAddresses.forString getOrElse loopbackIp,
    port getOrElse 0
  )

  def withDefaults(router: RouterConfig.Defaults): ServerConfig.Defaults =
    new ServerConfig.Defaults(this, router)
}

object ServerConfig {
  class Defaults(base: ServerConfig, router: RouterConfig.Defaults) {
    def ip = base.ip
    def port = base.port
    def addr = base.addr

    val MinValue = 1
    val MaxValue = math.pow(2, 16) - 1

    def validPort(port: Int): Boolean = MinValue <= port && port <= MaxValue

    def validated(others: Seq[Defaults]): CfgValidated[ConfigError, Validated] = {
      // TODO: unify this with code in Server.scala
      def conflicts(other: Defaults) = {
        val addr0 = other.addr
        val addr1 = this.addr
        val conflict = (addr1.getPort != 0) && (addr0.getPort == addr1.getPort) && {
          val (a0, a1) = (addr0.getAddress, addr1.getAddress)
          a0.isAnyLocalAddress || a1.isAnyLocalAddress || a0 == a1
        }
        if (conflict) Some(ConflictingPorts(addr0, addr1)) else None
      }

      port match {
        case Some(p) if !validPort(p) =>
          invalid(InvalidPort(p))
        case _ =>
          val allConflicts: Seq[ConflictingPorts] = others flatMap conflicts
          if (allConflicts.isEmpty) valid(new Validated(this)) else invalid(allConflicts)
      }
    }
  }

  class Validated(defaults: Defaults) {
    def addr = defaults.addr
  }

  def validateServers(
    servers: Seq[ServerConfig],
    router: RouterConfig.Defaults,
    previousServers: Seq[ServerConfig.Defaults]
  ): CfgValidated[ConfigError, Seq[ServerConfig.Validated]] = {
    ValidationUtils.transpose(servers.map(_.withDefaults(router).validated(previousServers)))
  }
}

case class BaseServerConfig(ip: Option[String] = None, port: Option[Int] = None) extends ServerConfig
