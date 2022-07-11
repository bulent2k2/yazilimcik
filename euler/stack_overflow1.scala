// worked (:-)
def sortLists[T](xs: List[T], ys: List[T])(implicit ord: math.Ordering[T]): Boolean = 
    if (xs.isEmpty || ys.isEmpty) xs.isEmpty
    else if (xs.head == ys.head) sortLists(xs.tail, ys.tail)
    else ord.lt(xs.head, ys.head)

val set1 = Set(List(1, 2, 4), List(1, 2), List(2, 1))
set1.toList.sortWith(sortLists)

val set2 = Set("abc".toList, "ab".toList, "ba".toList, List('a'), List('b'))
set2.toList.sortWith(sortLists)
