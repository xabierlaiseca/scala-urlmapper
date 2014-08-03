package me.laiseca.urlmapper

import java.util.StringTokenizer

import me.laiseca.urlmapper.trie.{NilTrie, Trie}

/**
 * Created by Xabier Laiseca on 20/07/14.
 */
class UrlMatcher {

  def matchUrl[T](url: String, paths: PathTrie[T]): List[UrlMapping[T]] = {

    def mappings(current: UrlSegment, rest: List[String], paths: PathTrie[T]) = {
      val subtrie = paths subtrie current
      if (subtrie.isEmpty) Nil else matchUrl(rest, subtrie)
    }

    def fixedMappings(segments: List[String], paths: PathTrie[T]): List[UrlMapping[T]] =
      mappings(new FixedValueUrlSegment(segments.head), segments.tail, paths)

    def wildcardMappings(segments: List[String], paths: PathTrie[T]): List[UrlMapping[T]] =
      mappings(WildcardUrlSegment, segments.tail, paths)

    def recursiveWildcardMapping(paths: PathTrie[T]): List[UrlMapping[T]] =
      mappings(RecursiveWildcardUrlSegment, Nil, paths)

    def matchUrl(segments: List[String], paths: PathTrie[T]): List[UrlMapping[T]] = segments match {
      case Nil => paths.value.toList
      case _ => recursiveWildcardMapping(paths) ::: fixedMappings(segments, paths) ::: wildcardMappings(segments, paths)
    }

    matchUrl(toSegments(url), paths)
  }
}
