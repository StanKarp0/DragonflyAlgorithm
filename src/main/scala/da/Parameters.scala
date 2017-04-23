package da

/**
  * Created by wojciech on 22.04.17. 
  */
abstract class Parameters {

  val w: Double // Old velocity weight
  val s: Double // Seperation weight
  val a: Double // Alignment weight
  val c: Double // Cohesion weight
  val f: Double // Food attraction weight
  val e: Double // Enemy distraction weight

}
