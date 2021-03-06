package me.laiseca.urlmapper

import me.laiseca.urlmapper.trie.Trie
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Xabier Laiseca on 28/07/14.
 */
class DefaultUrlMapperTest extends FlatSpec with Matchers {
  val ROOT_PATH = List.empty[UrlSegment]
  val ROOT = "root element"
  val ROOT_URL = "/"
  val ROOT_MAPPING = UrlMapping(ROOT_URL, ROOT_PATH, ROOT)
  val PETS_PATH = List(FixedValueUrlSegment("api"), FixedValueUrlSegment("pets"))
  val PETS = "There are 10 pets!!"
  val PETS_URL = "/api/pets"
  val PETS_MAPPING = UrlMapping(PETS_URL, PETS_PATH, PETS)
  val PET_INDIVIDUAL_PATH = List(FixedValueUrlSegment("api"), FixedValueUrlSegment("pets"), WildcardUrlSegment)
  val PET_INDIVIDUAL = "Pet individual"
  val PET_INDIVIDUAL_URL = PETS_URL + "/*"
  val PET_INDIVIDUAL_MAPPING = UrlMapping(PET_INDIVIDUAL_URL, PET_INDIVIDUAL_PATH, PET_INDIVIDUAL)
  val FOODS_PATH = List(FixedValueUrlSegment("api"), FixedValueUrlSegment("foods"), RecursiveWildcardUrlSegment)
  val FOODS = "Food for pets!!"
  val FOODS_BASE_URL = "/api/foods"
  val FOODS_URL = FOODS_BASE_URL + "/**"
  val FOODS_MAPPING = UrlMapping(FOODS_URL, FOODS_PATH, FOODS)

  val trie = Trie(ROOT_PATH -> ROOT_MAPPING,
    PETS_PATH -> PETS_MAPPING,
    PET_INDIVIDUAL_PATH -> PET_INDIVIDUAL_MAPPING,
    FOODS_PATH -> FOODS_MAPPING
  )

  val mapper = new DefaultUrlMapper[String](trie)

  "map" should "return the expected object for fully fixed url template match" in {
    mapper map PETS_URL should be { Some(PETS) }
  }

  it should "return the expected object for url template with wildcard" in {
    mapper map (PETS_URL + "/10") should be { Some(PET_INDIVIDUAL) }
  }

  it should "return the expected object for url template with recursive wildcard" in {
    mapper map (FOODS_BASE_URL + "/10/description") should be { Some(FOODS) }
  }

  it should "return the base mapping for '/'" in {
    mapper map ROOT_URL should be { Some(ROOT) }
  }

  it should "return no results when no matching template going through fully fixed path" in {
    mapper map "/api/unknown" should be { None }
  }

  it should "return no results when no matching template going through path with wildcard" in {
    mapper map (PETS_URL + "/10/unknown") should be { None }
  }
}
