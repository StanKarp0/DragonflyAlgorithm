package model

import da.Parameters

import scala.util.Random

/**
  * Created by wojciech on 22.04.17. 
  */
case class VariableParam(i: Int, max: Int) extends Parameters {

  def rand(): Double = Random.nextDouble()
  val w: Double = 0.9 - i * ((0.9-0.2)/max)
  private val my_c: Double = 0.1 - i * ((0.15-0.0)/(max/2.0)) match {
    case cc if cc < 0 => 0
    case cc => cc
  }
  val s: Double=0.1*my_c//2*rand()*my_c // Seperation weight
  val a: Double=0.1*my_c//2*rand()*my_c // Alignment weight
  val c: Double=0.7*my_c//2*rand()*my_c // Cohesion weight
  val f: Double=3.5*rand()      // Food attraction weight
  val e: Double=my_c          // Enemy distraction weight

}
