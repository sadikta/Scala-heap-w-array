import scala.math.log
case class Task(id : Int, name : String, var priority : Int){
  /**
    * overriding equals method
    *  if ids and names are equal than they are equal
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Task => that.id == this.id && that.name == this.name
      case _ => false
    }
  }

  /**
    * overriding hashCode method
    * @return a hash value using ids and names
    * equal tasks returns same hash value
    */
  override def hashCode:Int = {
    val prime = 31
    var result = 11
    result = prime * result + id
    result = prime * result + (if (name == null) 0 else name.hashCode)
    result
  }
}
class TaskQueue {

  private var array: Array[Task] = Array.ofDim(10)
  private var index: Int = 0

  /**
    * adding a task to the Queue
    * and arrange its place using its priority value
    * @param t is task
    */
  def add(t : Task): Unit = {
    adjustArray()
    array(index) = t
    arrangePlaceBottomUp(index)
    index += 1
  }

  /**
    * @return most prioritized task
    */
  def peek() : Option[Task] = {
    if(isEmpty) None
    else Some(array(0))
  }

  /**
    * @return most prioritized task and removes it
    */
  def poll() : Option[Task] = {
    if(isEmpty) None
    else {
      val temp = peek()
      remove(peek().get)
      index -= 1
      temp
    }
  }

  /**
    * Sets a priority to a task
    * @param t given task
    * @param p priority
    */
  def setPriority (t : Task, p : Int) : Unit = {
    array(find(t)).priority = p
  }

  /**
    * removes a selected task from Queue
    * @param t is the given task
    * @return true if removed, else false
    */
  def remove(t : Task) : Boolean = {
    if(isEmpty) false
    else {
      find(t) match {
        case -1 => false
        case taskIndex:Int => {
          array(taskIndex) = array(index-1)
          arrangePlaceTopDown(taskIndex)
          index -= 1
          true
        }
      }
    }
  }

  /**
    * @return the queue as a list
    */
  def toList : List[Task] = {
    array.toList.dropRight(array.length-index)
  }

  /**
    * finds the place of a task
    * @param t given task
    * @return task
    */
  private def find(t : Task) : Int = {
  array.indexOf(t)
  }

  /**
    * @return if the queue is empty
    */
  private def isEmpty : Boolean = index == 0

  /**
    * changes 2 elements indexes
    */  
  private def swap(firstIndex : Int, secondIndex : Int) : Unit = {
    val temp = array(firstIndex)
    array(firstIndex) = array(secondIndex)
    array(secondIndex) = temp
  }

  /**arranges elements place usıing priorities
    * @param placeIndex given index
    */
  private def arrangePlaceTopDown(placeIndex : Int) : Unit = {
    array(placeIndex).priority match {
      case y =>{
        val a = maxChild(placeIndex)
        if(array(a).priority>y){
          swap(maxChild(placeIndex),placeIndex)
          arrangePlaceTopDown(a)
        }
      }
    }
  }

  /**
    *
    * @param placeIndex
    */
  private def arrangePlaceBottomUp(placeIndex : Int) : Unit = {
    array(placeIndex).priority match {
      case y => {
        if(placeIndex > 1) {
          val a :Int = if(placeIndex % 2 == 0) (placeIndex-1)/2 else placeIndex/2
          if(array(placeIndex).priority > array(a).priority){
            swap(a,placeIndex)
            arrangePlaceBottomUp(a)
          }
        }
      }
    }
  }

  /**
    * find maxpriority child given index
    * @param indexParent parent index
    * @return maxchildindex
    */
  private def maxChild(indexParent : Int) : Int = {
    if((indexParent*2)+2<index){
      if(array((indexParent*2)+1).priority > array((indexParent*2)+2).priority) (indexParent*2)+1
      else (indexParent*2)+2
    } else if((indexParent*2)+2 == index) {
      (indexParent*2)+1
    }else{
      indexParent+1
    }
  }

  /**
    * adjusts array size if needed
    */
  private def adjustArray() : Unit = {
    if(index == array.length){
      val newArray = new Array[Task](2*array.length)
      Array.copy(array,0,newArray,0,array.length)
      array = newArray
    }
  }
}
//
//object TaskQueue{
//  def main(args: Array[String]) : Unit = {
//    val tas = new TaskQueue
//    val sad1 = Task(1,"sad1",100)
//    val sad2 = Task(1,"sad2",90)
//    val sad3 = Task(1,"sad3",80)
//    val sad4 = Task(1,"sad4",70)
//    val sad5 = Task(1,"sad5",60)
//    val sad6 = Task(1,"sad6",50)
//    val sad7 = Task(1,"sad7",40)
//    val sad8 = Task(1,"sad8",30)
//    val sad9 = Task(1,"sad9",20)
//    val sad10 = Task(1,"sad",85)
//    tas.add(sad1)
//    tas.add(sad2)
//    tas.add(sad3)
//    tas.add(sad4)
//    tas.add(sad5)
//    tas.add(sad6)
//    tas.add(sad10)
//    tas.add(sad8)
//    tas.add(sad9)
//    tas.remove(sad1)
//    tas.remove(sad2)
//    println(tas.peek().get.priority)
//    println(tas.find(sad3))
//    println()
//    tas.toList.foreach(a=>println(a))
//  }
//}