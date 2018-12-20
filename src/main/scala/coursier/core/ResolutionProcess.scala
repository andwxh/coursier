package coursier
package core

import coursier.util.Monad


sealed abstract class ResolutionProcess {
  def run[F[_]](
    fetch: Fetch.Metadata[F],
    maxIterations: Int
  )(implicit
    F: Monad[F]
  ): F[Resolution] =
    if (maxIterations == 0) F.point(???)
    else {
      val maxIterations0 =
        if (maxIterations > 0) maxIterations - 1 else maxIterations

      this match {
        case missing0 @ Missing(missing) =>
          F.bind(ResolutionProcess.fetchAll[F](missing, fetch)(F))(result =>
            missing0.next(result).run[F](fetch, maxIterations0)(F)
          )
      }
    }

  final def next[F[_]](
    fetch: Fetch.Metadata[F]
  )(implicit
    F: Monad[F]
  ): F[ResolutionProcess] =
    this match {
      case missing0 @ Missing(missing) =>
        F.map(ResolutionProcess.fetchAll(missing, fetch))(result => missing0.next(result))
    }
}

final case class Missing(missing: Seq[(Module, String)]) extends ResolutionProcess {

  def next(results: Fetch.MD): ResolutionProcess = ???

}

object ResolutionProcess {

  private[coursier] def fetchAll[F[_]](
    modVers: Seq[(Module, String)],
    fetch: Fetch.Metadata[F]
  )(implicit F: Monad[F]): F[Vector[((Module, String), Either[Seq[String], (Artifact.Source, Project)])]] =
    ???

}

