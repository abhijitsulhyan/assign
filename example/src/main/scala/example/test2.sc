val a = List(1,2,3)

a.head

def max(xs: List[Int]): Int = {
  def comp(v1:Int, v2:Int): Int = {
    if (v1 > v2) v1 else v2
  }

  if (xs.isEmpty) throw new java.util.NoSuchElementException("Empty list not allowed")
  else if (xs.tail.isEmpty) xs.head
  else max(comp(xs.head, xs.tail.head)::xs.tail.tail)
}

max(List(1,2,4))

