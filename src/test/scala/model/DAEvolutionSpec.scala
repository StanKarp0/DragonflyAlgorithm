package model

import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wojciech on 23.04.17. 
  */
class DAEvolutionSpec extends FlatSpec with Matchers {

  val lb: DenseVector[Double] = DenseVector[Double](-100, -100)
  val ub: DenseVector[Double] = DenseVector[Double](100, 100)


  "DA dynamic param " should " find (0,0) value 1.0" in {
    val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1) + 1.0
    val dragonflyAlgorithm = new DA(fit, 40, lb, ub, VariableParam)
    val result = dragonflyAlgorithm.iterator(1000).take(1000).toList
    val end = result.last
//    println(end.global.value) // result
//    result.map(r => s"${r.i} ${r.global.value}").foreach(println) // evolution global result
//    result.map(r => s"${r.i} ${r.actual}").foreach(println) // evolution actual result
//    result.map(r => s"${r.i} ${r.mean}").foreach(println) // evolution mean
//    result.map(r => s"${r.i} ${r.std}").foreach(println) // evolution std
    end.global.value shouldBe (1.0 +- 0.1)
  }


  "DA const" should " find (0,0) value 1.0" in {
    val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1) + 1.0
    val dragonflyAlgorithm = new DA(fit, 40/*Particles*/, lb, ub, (_, _) =>
      ConstParam(a = 1.0, c = 1.0, f = 1.0, e = 0.0, s = 0.0, w = 0)
    )
    val result = dragonflyAlgorithm.iterator(1000).take(1000).toList
    val end = result.last
    end.global.value shouldBe (1.0 +- 0.1)
  }

}
