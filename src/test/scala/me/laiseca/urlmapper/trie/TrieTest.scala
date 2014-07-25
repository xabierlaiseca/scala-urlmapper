package me.laiseca.urlmapper.trie

import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Xabier Laiseca on 16/07/14.
 */

class TrieTest extends FlatSpec with Matchers with MockitoSugar {
  import org.mockito.Mockito._

  class TestTrie[K, +V] extends Trie[K, V] {
    override protected[trie] def add[V1 >: V](kv: (List[K], Option[V1])): Trie[K, V1] = null
    override def -(key: List[K]): Trie[K, V] = ???
    override def get(key: List[K]): Option[V] = ???
    override def iterator: Iterator[(List[K], V)] = ???
    override def subtrie(node: K): Trie[K, V] = ???
    override def value: Option[V] = ???
  }

  "operator '+'" should "call 'add' with defined value" in {
    val newElement = List(1, 2) -> "value"
    val newTrie = mock[Trie[Int, String]]
    val testObj = mock[TestTrie[Int, String]](withSettings defaultAnswer CALLS_REAL_METHODS)

    when(testObj add (List(1, 2) -> Option("value"))) thenReturn newTrie

    (testObj + newElement) should be { newTrie }
  }

  it should "not call 'add' with empty value" in {
    val newElement = List(1, 2) -> null
    val testObj = mock[TestTrie[Int, String]](withSettings defaultAnswer CALLS_REAL_METHODS)

    (testObj + newElement) should be { testObj }
  }

  "apply" should "return empty map when 0 parameters are provided" in {
    Trie() should be { Trie.empty }
  }

  it should "create a new trie with the given elements when parameters are given" in {
    Trie(List(1) -> "1") should be {
      new NonEmptyTrie(Map(
        1 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1"))
      ), None)
    }
  }
}

class NilTrieTest extends FlatSpec with Matchers {
  "iterator" should "return empty iterator" in {
    NilTrie.iterator.isEmpty should be { true }
  }

  "get" should "return empty option" in {
    NilTrie.get(List.empty) should be { None }
  }

  "operator '+'" should "create a 1 element with 0 path length trie" in {
    NilTrie + (List.empty -> "1") should be {
      new NonEmptyTrie(Map(), Option("1"))
    }
  }

  it should "create a 1 element with 1 path length trie" in {
    Trie.empty[Int, String] + (List(1) -> "1") should be {
      new NonEmptyTrie(Map(
        1 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1"))
      ), None)
    }
  }

  "operator '-'" should "return an empty trie" in {
    NilTrie - List(1) should be { NilTrie }
  }

  "subtrie" should "return an empty trie" in {
    NilTrie subtrie 1 should be { NilTrie }
  }
}

class NonEmptyTrieTest extends FlatSpec with Matchers {
  val trie =  new NonEmptyTrie(Map(
      1 -> new NonEmptyTrie(Map(
        2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2"))
      ), None)
    ), Option("root")
  )

  "iterator" should "return an empty iterator for a zero element trie" in {
    new NonEmptyTrie[Int, Int](Map.empty, None).iterator.isEmpty should be { true }
  }

  it should "contain all elements in the trie" in {
    val it = trie.iterator

    it.next should be { Nil -> "root" }
    it.next should be { List(1, 2) -> "1 and 2" }
    it.hasNext should be { false }
  }

  "get" should "return the value for the given path" in {
    trie get List(1,2) should be {
      Option("1 and 2")
    }
  }

  it should "return the empty option for an incomplete path path" in {
    trie get List(1) should be {
      Option.empty[String]
    }
  }

  it should "return the empty option for a non existing path" in {
    trie get List(1, 4) should be {
      Option.empty[String]
    }
  }

  it should "return the empty option for too long path" in {
    trie get List(1, 2, 3) should be {
      Option.empty[String]
    }
  }

  "operator '+'" should "create a new trie with the new element when modifies value to existing node" in {
    trie + (List(1) -> "1") should be {
      new NonEmptyTrie(Map(
          1 -> new NonEmptyTrie(Map(
            2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2"))
          ), Option("1"))
        ), Option("root")
      )
    }
  }

  it should "create a new trie with the new element when creates a new branch" in {
    trie + (List(1, 3) -> "1 and 3") should be {
      new NonEmptyTrie(Map(
          1 -> new NonEmptyTrie(Map(
            2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2")),
            3 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 3"))
          ), None)
        ), Option("root")
      )
    }
  }

  it should "create a new trie with the new element when replaces existing non empty node" in {
    trie + (List(1, 2) -> "1 & 2") should be {
      new NonEmptyTrie(Map(
          1 -> new NonEmptyTrie(Map(
            2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 & 2"))
          ), None)
        ), Option("root")
      )
    }
  }

  it should "create a new trie with the new element when creates new leaf" in {
    trie + (List(1, 2, 3) -> "1 and 2 and 3") should be {
      new NonEmptyTrie(Map(
          1 -> new NonEmptyTrie(Map(
            2 -> new NonEmptyTrie(Map(
              3 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2 and 3"))
            ), Option("1 and 2"))
          ), None)
        ), Option("root")
      )
    }
  }

  "operator '-'" should "create a new trie without the element for root element" in {
    trie - List() should be {
      new NonEmptyTrie(Map(
          1 -> new NonEmptyTrie(Map(
            2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2"))
          ), None)
        ), None
      )
    }
  }

  it should "create a new trie without the element when node is leaf" in {
    val newTrie = trie - List(1, 2)
    newTrie should be {
      new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("root"))
    }

    newTrie.asInstanceOf[NonEmptyTrie[Int, String]].nodes should be { Map.empty }
  }

  it should "return empty trie when no nodes are left" in {
    new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("root")) - List() should be {
      Trie.empty
    }
  }

  "subtrie" should "return a subtrie when an existing node key is given" in {
    trie subtrie 1 should be {
      new NonEmptyTrie(Map(
        2 -> new NonEmptyTrie(Map.empty[Int, Trie[Int, String]], Option("1 and 2"))
      ), None)
    }
  }

  it should "return empty trie when a not existing node key is given" in {
    trie subtrie 2 should be { NilTrie }
  }
}