/**
  * a Scala implementation of the mergeSort algorithm
  */
class Sort {
  def mergeSort(xs: List[Int]): List[Int]= {
    val middle = xs.length / 2
    if (middle == 0) xs
    else {
      val (left, right) = xs.splitAt(middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case x :: xs1 => ys match {
      case Nil => xs
      case y :: ys1 => 
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    } 
  }
}
 

