package coursier.core

object Resolution {

  type ModuleVersion = (Module, String)

}

final case class Resolution(
  projectCache: Map[Resolution.ModuleVersion, (Artifact.Source, Project)],
  errorCache: Map[Resolution.ModuleVersion, Seq[String]]
) {

  def copyWithCache(
    errorCache: Map[Resolution.ModuleVersion, Seq[String]] = errorCache
  ): Resolution = ???

  def addToProjectCache(projects: (Resolution.ModuleVersion, (Artifact.Source, Project))*): Resolution = ???

  import Resolution._

  lazy val missingFromCache: Set[ModuleVersion] = ???


  lazy val isDone: Boolean = ???

  final def nextIfNoMissing: Resolution = ???

  def dependencyManagementMissing(project: Project): Set[ModuleVersion] = ???

}
