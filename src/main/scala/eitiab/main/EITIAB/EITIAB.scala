package eitiab.main.EITIAB

/**
  * Created by danielporter on 7/12/17.
  */

import shapeless.{HList, _}
import poly._
import shapeless.Generic._
trait VirtualizationService {
}

trait CloudProvider extends VirtualizationService {
  val pathToKey: String
}

case class VirtualBox() extends VirtualizationService

case class GCP(pathToKey: String) extends CloudProvider
case class Azure(pathToKey: String) extends CloudProvider
case class AWS(pathToKey: String) extends CloudProvider

trait OS
case object Windows10 extends OS {
}
case object Ubuntu1404 extends OS
case object Centos7 extends OS

case object VM {

}

case class VM(hostname: String = "localhost") {
  def running(os: OS) = {
    VMWithOS(hostname, os)
  }
}

case class VMWithOS(hostname: String, os: OS) {
  def on(vs: VirtualizationService) = {
    Environment(hostname, os, vs, Nil)
  }
}

trait ApplicationType {
  val host: Environment
}

trait VCS
case class Git(host: Environment) extends VCS

trait BuildApplication extends ApplicationType
trait ApplicationScaffold extends ApplicationType {
  def builtBy = AppBuiltBy
  val vcs: VCS
}

case class ScalaPlay26(host: Environment, vcs: VCS) extends ApplicationScaffold {
  def builtBy(builder: BuildApplication) = {
    AppBuiltBy(this, builder)
  }
}
trait ArtifactRepository extends ApplicationType
case class Nexus(host: Environment) extends ArtifactRepository

trait Packager
case object Docker extends Packager

case class AppBuiltBy(app: ApplicationScaffold, builder: BuildApplication) {
  def packagedInto(packager: Packager) = {
    AppBuiltAndPackaged(app, builder, packager)
  }
}

case class AppBuiltAndPackaged(app: ApplicationScaffold, builder: BuildApplication, packager: Packager) {
  def storedIn(artifacts: ArtifactRepository) = {
    AppBuiltAndPackagedAndStored(app, builder, packager, artifacts)
  }
}

case class AppBuiltAndPackagedAndStored(app: ApplicationScaffold, builder: BuildApplication, packager: Packager, artifacts: ArtifactRepository) {
  def deployedTo(envs: Environment*) = {
    BuildAndDeploy(this, envs)
  }
}

trait HealthMonitor
case class Consul(host: Environment) extends HealthMonitor

case class BuildAndDeploy(app: AppBuiltAndPackagedAndStored, targetEnvs: Seq[Environment]) {
  def monitoredBy(health: HealthMonitor) = {
    BuildDeployAndMonitor(app, targetEnvs, health)
  }
}

trait Logger extends ApplicationType

case class Splunk(host: Environment) extends Logger
case class BuildDeployAndMonitor(app: AppBuiltAndPackagedAndStored, targetEnvs: Seq[Environment], monitor: HealthMonitor) {
  def sendingLogsTo(logger: Logger) = {
    BuildDeployMonitorAndLog(app, targetEnvs, monitor, logger)
  }
}
case class BuildDeployMonitorAndLog(app: AppBuiltAndPackagedAndStored,
                                    targetEnvs: Seq[Environment],
                                    monitor: HealthMonitor,
                                    logger: Logger) {
  import shapeless.poly._
  def getEnvs: Set[Environment] = {

    Set(app.artifacts.host)
  }
}
case class Jenkins(host: Environment) extends BuildApplication

case class Environment(hostname: String,
                       os: OS,
                       vs: VirtualizationService,
                       applications: Seq[ApplicationType] = Nil) {
}



class eitab {
  def buildApplication(): Unit = {
    val gcp = GCP("/path/to/key")

    val webAppEnv =
      VM("WebApp") running
        Windows10 on gcp

    val jenkinsEnv =
      VM("Jenkins") running
        Ubuntu1404 on gcp

    val devEnv = VM("dev") running Ubuntu1404 on gcp
    val qaEnv = VM("qa") running Ubuntu1404 on gcp
    val stageEnv = VM("stage") running Ubuntu1404 on gcp
    val prod = VM("prod") running Ubuntu1404 on gcp

    val builtAndPackaged = ScalaPlay26(
      host = webAppEnv,
      vcs = Git(jenkinsEnv)
    ) builtBy
      Jenkins(jenkinsEnv) packagedInto
      Docker storedIn
      Nexus(jenkinsEnv) deployedTo
      (devEnv, qaEnv, stageEnv, prod) monitoredBy
      Consul(jenkinsEnv) sendingLogsTo Splunk(jenkinsEnv)

    
    val a = shapeless.Generic[BuildDeployMonitorAndLog]
    val b = a.to(builtAndPackaged).asInstanceOf[HList]
    val gen = shapeless.Generic[BuildDeployMonitorAndLog]

    val c = 1 :: 3 :: HNil


  }
}


