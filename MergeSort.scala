/**
  * a Scala implementation of the mergeSort algorithm
  */

object Merge  extends App {

  def mergeSort(integers: Vector[Int]): Vector[Int] = {
    val length = integers.length
    if (length < 2) integers
    else {
      val (left, right) = integers.splitAt((length + 1) / 2)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge(firstHalf: Vector[Int], secondHalf: Vector[Int]): Vector[Int] = {
    
    def iterMerge(firstHalf: Vector[Int], secondHalf :Vector[Int], mergeResult: Vector[Int]): Vector[Int] = {
      if (firstHalf.isEmpty) mergeResult ++ secondHalf
      else if (secondHalf.isEmpty) mergeResult ++ firstHalf
      else {      
        val firstHead = firstHalf.head
        val secondHead = secondHalf.head
        if (firstHead < secondHead) 
          iterMerge(firstHalf.drop(1), secondHalf, mergeResult :+ firstHead)
        else if (firstHead > secondHead)
          iterMerge(firstHalf, secondHalf.drop(1), mergeResult :+ secondHead)
        else iterMerge(firstHalf.drop(1), secondHalf.drop(1), mergeResult :+ firstHead :+ secondHead )
      } 
    }

    iterMerge(firstHalf, secondHalf, Vector[Int]())
  }

}