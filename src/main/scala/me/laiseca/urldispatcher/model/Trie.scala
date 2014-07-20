package me.laiseca.urldispatcher.model

import scala.collection.MapLike

/**
 * Created by Xabier Laiseca on 14/07/14.
 */
trait Trie[K, +V] extends Iterable[(List[K], V)] with Traversable[(List[K], V)] with Map[List[K], V] with MapLike[List[K], V, Trie[K, V]] {
  def +[V1 >: V](kv: (List[K], V1)): Trie[K, V1] =
    if(kv._2 == null) this else this add (kv._1 -> Option(kv._2))

  protected[model] def add[V1 >: V](kv: (List[K], Option[V1])): Trie[K, V1]
  def -(key: List[K]): Trie[K, V]

  override def empty = Trie.empty[K, V]
}

object Trie {
  def empty[K, V] = NilTrie.asInstanceOf[Trie[K, V]]
  def apply[K, V](elems: (List[K], V)*) = empty[K, V] ++ elems
}

private[model] case class NonEmptyTrie[K, V](nodes: Map[K, Trie[K, V]], value: Option[V]) extends Trie[K, V] {
  override def isEmpty = false

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

  override protected[model] def add[V1 >: V](kv: (List[K], Option[V1])): Trie[K, V1] = kv._1 match {
    case Nil => new NonEmptyTrie(nodes, kv._2)
    case e :: es => new NonEmptyTrie(
      nodes + (e -> (nodes.getOrElse(e, Trie.empty[K, V1]) add (es -> kv._2))),
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
  override def isEmpty = true

  override def iterator: Iterator[(List[Any], Nothing)] = Iterator.empty

  override def get(key: List[Any]): Option[Nothing] = None

  override protected[model] def add[V1 >: Nothing](kv: (List[Any], Option[V1])): Trie[Any, V1] = kv._1 match {
    case Nil => new NonEmptyTrie[Any, V1](Map.empty, kv._2)
    case e :: es => new NonEmptyTrie[Any, V1](Map(e -> (NilTrie add (es -> kv._2))), None)
  }

  override def -(key: List[Any]): Trie[Any, Nothing] = this
}

