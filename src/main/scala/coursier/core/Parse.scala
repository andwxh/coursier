package coursier.core

import java.util.regex.Pattern.quote

object Parse {

  def version(s: String): Option[Version] = {
    val trimmed = s.trim
    if (trimmed.isEmpty || trimmed.exists(c => c != '.' && c != '-' && c != '_' && !c.isLetterOrDigit)) None
    else Some(Version(trimmed))
  }

  def versionInterval(s: String): Option[VersionInterval] = {

    def parseBounds(fromIncluded: Boolean, toIncluded: Boolean, s: String) = {

      val commaIdx = s.indexOf(',')

      if (commaIdx >= 0) {
        val strFrom = s.take(commaIdx)
        val strTo = s.drop(commaIdx + 1)

        for {
          from <- if (strFrom.isEmpty) Some(None) else version(strFrom).map(Some(_))
          to <- if (strTo.isEmpty) Some(None) else version(strTo).map(Some(_))
        } yield VersionInterval(from.filterNot(_.isEmpty), to.filterNot(_.isEmpty), fromIncluded, toIncluded)
      } else if (s.nonEmpty && fromIncluded && toIncluded)
        for (v <- version(s) if !v.isEmpty)
          yield VersionInterval(Some(v), Some(v), fromIncluded, toIncluded)
      else
        None
    }

    for {
      fromIncluded <- if (s.startsWith("[")) Some(true) else if (s.startsWith("(")) Some(false) else None
      toIncluded <- if (s.endsWith("]")) Some(true) else if (s.endsWith(")")) Some(false) else None
      s0 = s.drop(1).dropRight(1)
      itv <- parseBounds(fromIncluded, toIncluded, s0)
    } yield itv
  }

  private val multiVersionIntervalSplit = ("(?<=[" + quote("])") + "]),(?=[" + quote("([") + "])").r

  def multiVersionInterval(s: String): Option[VersionInterval] = {

    // TODO Use a full-fledged (fastparsed-based) parser for this and versionInterval above

    val openCount = s.count(c => c == '[' || c == '(')
    val closeCount = s.count(c => c == ']' || c == ')')

    if (openCount == closeCount && openCount >= 1)
      versionInterval(multiVersionIntervalSplit.split(s).last)
    else
      None
  }

  def versionConstraint(s: String): VersionConstraint = {
    def noConstraint = if (s.isEmpty) Some(VersionConstraint.all) else None

    noConstraint
      .orElse(versionInterval(s).orElse(multiVersionInterval(s)).map(VersionConstraint.interval))
      .getOrElse(VersionConstraint.preferred(Version(s)))
  }

  val fallbackConfigRegex = {
    val noPar = "([^" + quote("()") + "]*)"
    "^" + noPar + quote("(") + noPar + quote(")") + "$"
  }.r

  // TODO Make that a method of Configuration?
  def withFallbackConfig(config: Configuration): Option[(Configuration, Configuration)] =
    Parse.fallbackConfigRegex.findAllMatchIn(config.value).toSeq match {
      case Seq(m) =>
        assert(m.groupCount == 2)
        val main = Configuration(config.value.substring(m.start(1), m.end(1)))
        val fallback = Configuration(config.value.substring(m.start(2), m.end(2)))
        Some((main, fallback))
      case _ =>
        None
    }

}
