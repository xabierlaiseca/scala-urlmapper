package me.laiseca.urlmapper

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Xabier Laiseca on 26/07/14.
 */
class DefaultUrlMappingSelectionAlgorithmTest extends FlatSpec with Matchers {
  val FIXED_URL_MAPPING = new UrlMapping("/pets/count",
    List(FixedValueUrlSegment("pets"), FixedValueUrlSegment("count")), "fixed")
  val WILDCARD_URL_MAPPING_1 = new UrlMapping("/pets/*",
    List(FixedValueUrlSegment("pets"), WildcardUrlSegment), "wildcard 1")
  val WILDCARD_URL_MAPPING_2 = new UrlMapping("/*/count",
    List(WildcardUrlSegment, FixedValueUrlSegment("count")), "wildcard 2")
  val WILDCARD_URL_MAPPING_3 = new UrlMapping("/*/*",
    List(WildcardUrlSegment, WildcardUrlSegment), "wildcard 3")
  val RECURSIVE_WILDCARD_URL_MAPPING_1 = new UrlMapping("/pets/**",
    List(FixedValueUrlSegment("pets"), RecursiveWildcardUrlSegment), "recursive wildcard 1")
  val RECURSIVE_WILDCARD_URL_MAPPING_2 = new UrlMapping("/**",
    List(RecursiveWildcardUrlSegment), "recursive wildcard 2")

  "select" should "throw an exception for empty list" in {
    intercept[IllegalArgumentException]{
      new DefaultUrlMappingSelectionAlgorithm().select(List())
    }
  }
  
  "select" should "return the only value when one mapping provided" in {
    new DefaultUrlMappingSelectionAlgorithm().select(List(FIXED_URL_MAPPING)) should be { FIXED_URL_MAPPING }
  }
  
  "select" should "return the value of the fixed url mapping" in {
    new DefaultUrlMappingSelectionAlgorithm().select(
      List(FIXED_URL_MAPPING, WILDCARD_URL_MAPPING_1, WILDCARD_URL_MAPPING_2, WILDCARD_URL_MAPPING_3,
        RECURSIVE_WILDCARD_URL_MAPPING_1, RECURSIVE_WILDCARD_URL_MAPPING_2)
    ) should be { FIXED_URL_MAPPING }
  }

  "select" should "return the value of the wildcard url mapping which start with fixed segments" in {
    new DefaultUrlMappingSelectionAlgorithm().select(
      List(WILDCARD_URL_MAPPING_1, WILDCARD_URL_MAPPING_2, WILDCARD_URL_MAPPING_3)
    ) should be { WILDCARD_URL_MAPPING_1 }
  }

  "select" should "return the value of the fixed url mapping which got fixed segment after wildcard" in {
    new DefaultUrlMappingSelectionAlgorithm().select(
      List(WILDCARD_URL_MAPPING_2, WILDCARD_URL_MAPPING_3)
    ) should be { WILDCARD_URL_MAPPING_2 }
  }

  "select" should "return the value of the recursive wildcard url mapping which start with fixed segments" in {
    new DefaultUrlMappingSelectionAlgorithm().select(
      List(RECURSIVE_WILDCARD_URL_MAPPING_1, RECURSIVE_WILDCARD_URL_MAPPING_2)
    ) should be { RECURSIVE_WILDCARD_URL_MAPPING_1 }
  }

  "select" should "return the value of the wildcard url mapping over the one with recursive wildcard at same position" in {
    new DefaultUrlMappingSelectionAlgorithm().select(
      List(WILDCARD_URL_MAPPING_1, RECURSIVE_WILDCARD_URL_MAPPING_1)
    ) should be { WILDCARD_URL_MAPPING_1 }
  }
}
