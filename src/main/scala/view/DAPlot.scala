package view

import breeze.linalg.DenseVector
import breeze.plot.{Figure, plot}
import da.Parameters
import model.ConstParam

/**
  * Created by wojciech on 23.04.17. 
  */
object DAPlot {

  def evolution(i: List[Int], global: Option[List[Double]], actual: Option[List[Double]], mean: Option[List[Double]], name: String = "test.png"): Unit = {
    val iterations = DenseVector(i.map(_.toDouble).toArray)
    val f = Figure.apply("Proces ewolucji")
    val p = f.subplot(0)
    global.map(_.toArray).map(DenseVector(_)).foreach(v => p += plot(iterations, v, name = "Optimum globalne"))
    actual.map(_.toArray).map(DenseVector(_)).foreach(v => p += plot(iterations, v, name = "Wartość aktualna"))
    mean.map(_.toArray).map(DenseVector(_)).foreach(v => p += plot(iterations, v, name = "Wartość średnia"))
    p.title = "Proces ewolucji"
    p.legend = true
    p.xlabel = "Liczba iteracji"
    p.ylabel = "Wartość funkcji"
    //f.saveas(name)
  }

  def evolutionStdDev(i: List[Int], stddev: List[Double], name: String = "test.png"): Unit = {
    val iterations = DenseVector(i.map(_.toDouble).toArray)
    val f = Figure.apply("Odchylenie standardowe")
    val p = f.subplot(0)
    p += plot(iterations, DenseVector(stddev.toArray))
    p.title = "Odchylenie standardowe"
    p.xlabel = "Liczba iteracji"
    p.ylabel = "o"
    //f.saveas(name)
  }

  def parameters(parameters: (Int, Int) => Parameters, tests: Int, maxI: Int): Unit = {
    val params = List.range(0, maxI).map{ i =>
      val t = List.fill(tests)(i).map(parameters(_, maxI))
      ConstParam(
        t.map(_.a).sum/tests,
        t.map(_.c).sum/tests,
        t.map(_.e).sum/tests,
        t.map(_.f).sum/tests,
        t.map(_.s).sum/tests,
        t.map(_.w).sum/tests
      )
    }
    val f = Figure.apply("Wartości parametrów")
    val p = f.subplot(0)
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.a).toArray), name = "a")
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.c).toArray), name = "c")
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.e).toArray), name = "e")
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.f).toArray), name = "f")
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.s).toArray), name = "s")
    p += plot(DenseVector.rangeD(0.0, maxI), DenseVector(params.map(_.w).toArray), name = "w")
    p.legend = true
    p.title = "Wartości parametrów"
    p.xlabel = "Liczba iteracji"
  }



}
