package view

import breeze.linalg.DenseVector
import model.{ConstParam, DA, VariableParam}

/**
  * Created by wojciech on 23.04.17. 
  */
object MainPlot extends App {

  val lb = DenseVector[Double](-100, -100)
  val ub = DenseVector[Double](100, 100)
  def parameters(iteration: Int, maxIteration: Int) =
//    ConstParam(a = 2.0, c = 1.0, e = 1.0, f = 1.0, s = 0.0, w = 0.0)
    VariableParam(iteration, maxIteration)

  val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1) + 1.0
  val da = new DA(fit, 100, lb, ub, parameters)
  val result = da.iterator(1000).take(1000).toList

  DAPlot.evolution(
    result.map(_.i),
    Some(result.map(_.global.value)),
    Some(result.map(_.actual)),
    Some(result.map(_.mean)),
    "evolution_1.png"
  )
  DAPlot.evolutionStdDev(
    result.map(_.i),
    result.map(_.std),
    "std_dev_1.png"
  )
  DAPlot.parameters(parameters, 1000/*Liczba testow dla jednej iteracji; przy prawdopdb*/, 1000)
//  result.map(r => s"${r.i} ${r.global.value}").foreach(println) // evolution global result
//  result.map(r => s"${r.i} ${r.actual}").foreach(println) // evolution actual result
//  result.map(r => s"${r.i} ${r.mean}").foreach(println) // evolution mean
//  result.map(r => s"${r.i} ${r.std}").foreach(println) // evolution std
  println(result.last.global.x, result.last.global.value)

}
