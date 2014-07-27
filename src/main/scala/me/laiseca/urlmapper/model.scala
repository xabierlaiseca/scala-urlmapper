package me.laiseca.urlmapper

/**
 * Created by Xabier Laiseca on 25/07/14.
 */

case class UrlMapping[T](template: String, segments: List[UrlSegment], value: T)

trait UrlSegment
case class FixedValueUrlSegment(segment: String) extends UrlSegment
object WildcardUrlSegment extends UrlSegment
object RecursiveWildcardUrlSegment extends UrlSegment