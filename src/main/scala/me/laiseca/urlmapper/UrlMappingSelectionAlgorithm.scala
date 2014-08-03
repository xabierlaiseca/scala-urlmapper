package me.laiseca.urlmapper

/**
 * Created by Xabier Laiseca on 25/07/14.
 */
trait UrlMappingSelectionAlgorithm {
  def select[T](mappings: List[UrlMapping[T]]): UrlMapping[T]
}

class DefaultUrlMappingSelectionAlgorithm extends UrlMappingSelectionAlgorithm {
  override def select[T](mappings: List[UrlMapping[T]]): UrlMapping[T] = {

    def selectRec(mappings: (List[UrlSegment], UrlMapping[T])*): UrlMapping[T] = {
      val x :: xs = mappings

      xs match {
        case Nil => x._2
        case _ => selectRec(mappings.groupBy(
          mapping => mapping._1.head match {
            case WildcardUrlSegment => UrlMappingType.Wildcard
            case RecursiveWildcardUrlSegment => UrlMappingType.RecursiveWildcard
            case _ => UrlMappingType.Fixed
          }
        ).minBy(elem => elem._1.id)._2.map(
          current => current._1.tail -> current._2
        ):_*)
      }
    }

    mappings match {
      case Nil => throw new IllegalArgumentException
      case _ => selectRec(mappings map {
        elem => elem.segments -> elem
      }:_*)
    }
  }
}

private object UrlMappingType extends Enumeration {
  type UrlMappingType = Value
  val Fixed, Wildcard, RecursiveWildcard = Value
}