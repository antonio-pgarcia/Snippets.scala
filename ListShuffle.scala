package org.holistic.snippets

object ListShuffle {
  
  // -- | Emulation of Haskell fst (Lisp car)
  def fst(l: (List[Any],List[Any])): List[Any] = l._1
  def fst(l: List[Any]): Any = {
    l.head
  }

  // -- | Emulation of Haskell snd (Lisp cdr)
  def snd(l: (List[Any],List[Any])): List[Any] = l._2
  def snd(l: List[Any]): List[Any] = {
    l.tail
  }
  
  // -- | Pop one element from a specific list position
  def pop(i: Int, l: List[Any]) : (Any,List[Any]) = {
    l match {
        case Nil => (Nil, Nil)
        case List(_*) => {
          val f = fst(l.splitAt(i-1))
          val s = snd(l.splitAt(i-1))
          val e = if (!s.isEmpty) s.head else None 
          val ll = f ::: s.drop(1)
          (e,ll)  
        }
    }
  }

  // -- | Generate random number within interval a-b with seed s
  def myrandom(a: Int, b: Int, s: Int) : Int = {
    val r = new scala.util.Random
    val range = a to b
    r.setSeed(s)
    range(r.nextInt(range length))
  }
  
  // -- | Shuffle a list
  def shuffle(l: List[Any]) : List[Any] = {
    shufflep(myrandom(1,l.size,0),l,Nil)
  }
  
  // -- | Shuffle' auxiliary
  def shufflep(r: Int, l1: List[Any], l2: List[Any]): List[Any] = {
    val len = l1.size
    val t = pop(r,l1)
    if (len == 0) l2
    else 
      shufflep(myrandom(1,len,r), t._2, if (t._1 != None) l2 ::: List(t._1) else l2)
  } 
  
  // -- | The main entry point.
  def main(args: Array[String]) {
	  val v: List[Int] = List.range(1,101)
	  println("List shuffle!")
	  println(shuffle(v))
  }
  
}
