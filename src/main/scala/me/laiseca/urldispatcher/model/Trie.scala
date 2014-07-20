package me.laiseca.urldispatcher.model

import scala.collection.MapLike

/**
 * Created by xabier on 14/07/14.
 */
trait Trie[K, +V] extends Iterable[(List[K], V)] with Traversable[(List[K], V)] with Map[List[K], V] with MapLike[List[K], V, Trie[K, V]] {
  def +[V1 >: V](kv: (List[K], V1)): Trie[K, V1]
  def -(key: List[K]): Trie[K, V]

  override def empty = NilTrie.asInstanceOf[Trie[K, V]]
}

object Trie {
  def empty[K, V] = NilTrie.asInstanceOf[Trie[K, V]]
}

private[model] case class NonEmptyTrie[K, V](nodes: Map[K, Trie[K, V]], value: Option[V]) extends Trie[K, V] {
  override def iterator: Iterator[(List[K], V)] = value.foldLeft {
    for {
      node <- nodes.toStream
      tuple <- node._2
    } yield (node._1 :: tuple._1) -> tuple._2
  } ( (acc, current) => (Nil -> current) #:: acc ).iterator

  override def get(path: List[K]): Option[V] = path match {
    case Nil => value
    case e :: es => if(nodes contains path.head) nodes(path.head) get path.tail
                    else None
  }

  override def +[V1 >: V](kv: (List[K], V1)): Trie[K, V1] = kv._1 match {
    case Nil => new NonEmptyTrie(nodes, Option(kv._2))
    case e :: es => new NonEmptyTrie(
      nodes + (e -> (nodes.getOrElse(e, Trie.empty[K, V1]) + (es -> kv._2))),
      value)
  }

  override def -(key: List[K]): Trie[K, V] = key match {
    case Nil => if(nodes.isEmpty) empty
                else new NonEmptyTrie(nodes, None)
    case e :: es => if(nodes contains e) {
                      val subtrie = nodes(e) - es
                      if(subtrie.isEmpty && value.isEmpty) subtrie
                      else if (subtrie.isEmpty) new NonEmptyTrie(nodes - e, value)
                      else new NonEmptyTrie(nodes + (e -> subtrie), value)
                    } else this
  }
}

private[model] object NilTrie extends Trie[Any, Nothing] {
  override def iterator: Iterator[(List[Any], Nothing)] = Iterator.empty

  override def get(key: List[Any]): Option[Nothing] = None

  override def +[V1 >: Nothing](kv: (List[Any], V1)): Trie[Any, V1] = kv._1 match {
    case Nil => new NonEmptyTrie[Any, V1](Map.empty, Option(kv._2))
    case e :: es => new NonEmptyTrie[Any, V1](Map(e -> (NilTrie + (es -> kv._2))), None)
  }

  override def -(key: List[Any]): Trie[Any, Nothing] = this
}

