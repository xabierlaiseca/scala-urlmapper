package me.laiseca.urlmapper

import me.laiseca.urlmapper.trie.Trie

/**
 * Created by Xabier Laiseca on 27/07/14.
 */
trait UrlMapper[T] {
  def map(url: String): Option[T]
}

object UrlMapper {
  private def toPathTrie[T] (wildcard: String, recursiveWildcard: String, mappings: (String, T)*): PathTrie[T] =
    Trie((for {
      mapping <- mappings
      segments = toUrlSegments(mapping._1, wildcard, recursiveWildcard)
    } yield segments -> UrlMapping(mapping._1, segments, mapping._2)):_*)

  def apply[T](paths: PathTrie[T]): UrlMapper[T] = new DefaultUrlMapper(paths)
  def apply[T](paths: PathTrie[T], selectionAlgorithm: UrlMappingSelectionAlgorithm): UrlMapper[T] =
    new CustomizableUrlMapper(paths, selectionAlgorithm)

  def apply[T](wildcard: String, recursiveWildcard: String, mappings: (String, T)*): UrlMapper[T] =
    apply(toPathTrie(wildcard, recursiveWildcard, mappings:_*))

  def apply[T](wildcard: String, recursiveWildcard: String, selectionAlgorithm: UrlMappingSelectionAlgorithm,
               mappings: (String, T)*): UrlMapper[T] =
    apply(toPathTrie(wildcard, recursiveWildcard, mappings:_*), selectionAlgorithm)

  def apply[T](mappings: (String, T)*): UrlMapper[T] = apply("*", "**", mappings:_*)
  def apply[T]( selectionAlgorithm: UrlMappingSelectionAlgorithm, mappings: (String, T)*): UrlMapper[T] =
    apply("*", "**", selectionAlgorithm, mappings:_*)
}

class CustomizableUrlMapper[T] private[urlmapper] (val paths: PathTrie[T],
    val selectionAlgorithm: UrlMappingSelectionAlgorithm) extends UrlMapper[T] {
  private val matcher = new UrlMatcher

  override def map(url: String): Option[T] =
    matcher.matchUrl(url, paths) match {
      case Nil => None
      case options: List[UrlMapping[T]] => Some(selectionAlgorithm.select[T](options).value)
    }
}

class DefaultUrlMapper[T] private[urlmapper] (val paths: PathTrie[T]) extends UrlMapper[T] {
  override def map(url: String): Option[T] = {
    def mapping(current: UrlSegment, rest: List[String], paths: PathTrie[T]) = {
      val subtrie = paths subtrie current
      if (subtrie.isEmpty) None else map(rest, subtrie)
    }

    def fixedMappings(segments: List[String], paths: PathTrie[T]): Option[UrlMapping[T]] =
      mapping(new FixedValueUrlSegment(segments.head), segments.tail, paths)

    def wildcardMappings(segments: List[String], paths: PathTrie[T]): Option[UrlMapping[T]] =
      mapping(WildcardUrlSegment, segments.tail, paths)

    def recursiveWildcardMapping(paths: PathTrie[T]): Option[UrlMapping[T]] =
      mapping(RecursiveWildcardUrlSegment, Nil, paths)

    def map(segments: List[String], paths: PathTrie[T]): Option[UrlMapping[T]] = segments match {
      case Nil => paths.value
      case _ => fixedMappings(segments, paths).
        orElse(wildcardMappings(segments, paths)).
        orElse(recursiveWildcardMapping(paths))
    }

    map(toSegments(url), paths).map(_.value)
  }
}
