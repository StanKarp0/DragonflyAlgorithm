package model

import breeze.linalg.DenseVector
import breeze.stats.distributions.Uniform
import da.{DragonflyAlgorithm, Parameters}
import da.Tools.V

/**
  * Created by wojciech on 22.04.17. 
  */
class DA(f: DenseVector[Double] => Double, val nAgents: Int, lb: DenseVector[Double], ub: DenseVector[Double], parameters: (Int, Int) => Parameters)
  extends DragonflyAlgorithm(nAgents) {

  override def func(x: V): Double = f(x)

  override def radius(i: Int, max: Int): V = {
    val bounds = ub - lb
    (bounds *:* 0.25) + (bounds *:* ((i.toDouble / max.toDouble) * 2.0))
  }
  private val borders = lb.data.zip(ub.data).map(t => Border.square(t._1, t._2))

  def border(pos: V, delta: V): (V, V) = {
    val (newP, newD) = pos.data.zip(delta.data).zip(borders).map{
      case ((v, d), b) =>
        val nv = b(v)
        ( nv, if(nv == v) d else v)
    }.unzip

//    val (newP, newD) = pos.data.zip(delta.data).zip(lb.data.zip(ub.data)).map{
//      case ((v, _), (l, u)) if v < l => (u, 0.0)
//      case ((v, _), (l, u)) if v > u => (l, 0.0)
//      case ((v, d), _) => (v, d)
//    }.unzip
    (DenseVector(newP), DenseVector(newD))
  }

  override def params(i: Int, max: Int): Parameters = parameters(i, max)

  override def randomAgents(): Iterator[Point] = {
    val uniform = lb.data.zip(ub.data).map(t => new Uniform(t._1, t._2))
    def vector(): DenseVector[Double] = DenseVector.apply(uniform.map(_.get()))
    Iterator.continually(Point(vector(), vector()))
  }

}
object Border {
  def square(lower: Double, upper: Double): Double => Double = {
    val diff = upper - lower
    (x: Double) =>
      (if (math.floor((math.abs(x) + upper) / diff) % 2 == 1) {
        upper - math.abs(x + upper) % diff
      } else {
        math.abs(x + upper) % diff - upper
      }) * (if (x < lower) -1 else 1)
  }
}
