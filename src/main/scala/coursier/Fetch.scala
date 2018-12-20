package coursier

import coursier.core.{Artifact, Module, Project}

object Fetch {

  type MD = Seq[(
    (Module, String),
    Either[Seq[String], (Artifact.Source, Project)]
  )]

  type Metadata[F[_]] = Seq[(Module, String)] => F[MD]

}
