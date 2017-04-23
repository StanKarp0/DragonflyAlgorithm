package view

import breeze.linalg.DenseVector
import model.{DA, VariableParam}

/**
  * Created by wojciech on 23.04.17. 
  */
object MainMean extends App {
  val nAgents = 40
  val iterations = 1000
  val tests = 10
  def parameters(iteration: Int, maxIteration: Int) =
    VariableParam(iteration, maxIteration)


  val lb: DenseVector[Double] = DenseVector[Double](-100, -100)
  val ub: DenseVector[Double] = DenseVector[Double](100, 100)
  val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1) + 1.0
  val results = List.fill(tests)(new DA(fit, nAgents, lb, ub, parameters))
    .map(_.iterator(iterations).take(iterations).toList.last)

  val values = DenseVector(results.map(_.global.value).toArray)
  val mean = breeze.stats.mean(values)
  val stddev = breeze.stats.stddev(values)
  val meanOfPopulation = DenseVector(results.map(_.mean).toArray)
  val meanPop = breeze.stats.mean(meanOfPopulation)

  println(s"Mean = $mean")
  println(s"StdDev = $stddev")
  println(s"MeanPop = $meanPop")

}
