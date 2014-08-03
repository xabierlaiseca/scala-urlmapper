package me.laiseca

import java.util.StringTokenizer

import me.laiseca.urlmapper.trie.Trie

/**
 * Created by Xabier Laiseca on 27/07/14.
 */
package object urlmapper {
  type PathTrie[T] = Trie[UrlSegment, UrlMapping[T]]

  def toUrlSegments(url: String, wildcard: String, recursiveWildcard: String) = toSegments(url) map {
    case `wildcard` => WildcardUrlSegment
    case `recursiveWildcard` => RecursiveWildcardUrlSegment
    case other => FixedValueUrlSegment(other)
  }

  def toSegments(url: String) = {
    import scala.collection.JavaConversions._
    new StringTokenizer(url, "/").foldRight(List[String]()) {
      (current, list) => current.asInstanceOf[String] :: list
    }
  }
}
