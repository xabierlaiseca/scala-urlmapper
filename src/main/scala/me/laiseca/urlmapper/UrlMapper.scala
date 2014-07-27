package me.laiseca.urlmapper

import java.util.StringTokenizer

import me.laiseca.urlmapper.trie.{NilTrie, Trie}

/**
 * Created by Xabier Laiseca on 20/07/14.
 */
class UrlMapper[T](val paths: Trie[UrlSegment, UrlMapping[T]]) {
  import UrlMapper._

  private type PathTrie = Trie[UrlSegment, UrlMapping[T]]

  def map(url: String): List[UrlMapping[T]] = {

    def mappings(current: UrlSegment, rest: List[String], paths: PathTrie) = {
      val subtrie = paths subtrie current
      if (subtrie.isEmpty) Nil else map(rest, subtrie)
    }

    def fixedMappings(segments: List[String], paths: PathTrie): List[UrlMapping[T]] =
      mappings(new FixedValueUrlSegment(segments.head), segments.tail, paths)

    def wildcardMappings(segments: List[String], paths: PathTrie): List[UrlMapping[T]] =
      mappings(WildcardUrlSegment, segments.tail, paths)

    def recursiveWildcardMapping(paths: PathTrie): List[UrlMapping[T]] =
      mappings(RecursiveWildcardUrlSegment, Nil, paths)

    def map(segments: List[String], paths: PathTrie): List[UrlMapping[T]] = segments match {
      case Nil => paths.value.toList
      case _ => recursiveWildcardMapping(paths) ::: fixedMappings(segments, paths) ::: wildcardMappings(segments, paths)
    }

    map(toSegments(url), paths)
  }
}

object UrlMapper {
  def apply[T](paths: Trie[UrlSegment, UrlMapping[T]]): UrlMapper[T] = new UrlMapper(paths)

  def apply[T](wildcard: String, recursiveWildcard: String, mappings: (String, T)*): UrlMapper[T] = apply {
    val elements = for {
      mapping <- mappings
      segments = toUrlSegments(mapping._1, wildcard, recursiveWildcard)
    } yield segments -> UrlMapping(mapping._1, segments, mapping._2)

    Trie(elements:_*)
  }

  def apply[T](mappings: (String, T)*): UrlMapper[T] = apply("*", "**", mappings:_*)

  private def toUrlSegments(url: String, wildcard: String, recursiveWildcard: String) = toSegments(url) map {
    case `wildcard` => WildcardUrlSegment
    case `recursiveWildcard` => RecursiveWildcardUrlSegment
    case other => FixedValueUrlSegment(other)
  }

  private def toSegments(url: String) = {
    import scala.collection.JavaConversions._
    new StringTokenizer(url, "/").foldRight(List[String]()) {
      (current, list) => current.asInstanceOf[String] :: list
    }
  }
}
