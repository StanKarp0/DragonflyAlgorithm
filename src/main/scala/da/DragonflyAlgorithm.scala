package da

import breeze.linalg.{DenseVector, norm}
import da.Tools.V

import scala.util.Random

/**
  * Created by wojciech on 22.04.17. 
  */
abstract class DragonflyAlgorithm(nAgents: Int) {

  def func(x: V): Double
  def randomAgents(): Iterator[Point]
  def radius(i: Int, max: Int): V
  def params(i: Int, max: Int): Parameters
  def border(pos: V, delta: V): (V, V)

  case class Point(x: V, delta: V, history: List[V] = Nil) {
    val value: Double = func(x)
    def next(v: V, d: V): Point = Point(v, d, Nil)
  }
  case class Result(i: Int, global: Point, actual: Double, food: V, enemy: V, radius: V, agents: List[Point], mean: Double = 0, std: Double = 0)

  def iterator(max: Int): Iterator[Result] = {
    // Initialize the dragonflies population Xi (i = 1, 2, ..., n)
    // Initialize step vectors ΔXi (i = 1, 2, ..., n)
    val init: List[Point] = randomAgents().take(nAgents).toList
    val minimum: Point = init.minBy(_.value)
    val maximum: V = init.maxBy(_.value).x
    val start = Result(1, minimum, minimum.value, minimum.x, maximum, radius(0, max), init)

    Iterator.iterate(start){
      case result@ Result(i, global, _, _, _, _, agents, _, _) =>

        // Update the food source and enemy
        val newFood = agents.minBy(_.value).x
        val newEnemy = agents.maxBy(_.value).x

        // Update w, s, a, c, f, and e
        val p = params(i, max)

        // Update neighbouring radius
        val rad = radius(i, max)

        val newAgents = agents.map{ agent =>
          // Find neighbours
          val neighbours = agents.filter(a => Tools.inRange(rad, a.x, agent.x))

          // if a dragonfly has at least one neighbouring dragonfly
          val (newP, newV) = if(neighbours.nonEmpty) {
            val nV = Tools.updateVelocity(rad, neighbours.map(_.delta), neighbours.map(_.x),
              newFood, newEnemy, agent.x, agent.delta, p) // Eq. 3.6
            val nP = Tools.updatePosition(agent.x, nV) // Eq. 3.7
            (nP, nV)
          } else {
            val nV = agent.delta *:* 0.0
            val nP = Tools.updateLevy(agent.x) // Eq. 3.8
            (nP, nV)
          }

          // Check and correct the new positions based on the boundaries of variables
          val (newPb, newVb) = border(newP, newV)
          agent.next(newPb, newVb)
        }
        val newMinimum = newAgents.minBy(_.value)
        val newGlobal = if(global.value > newMinimum.value) newMinimum else global
        val actorVector = DenseVector(newAgents.map(_.value).toArray)
        val newMean = breeze.stats.mean(actorVector)
        val newStd = breeze.stats.stddev(actorVector)
        result.copy(i + 1, newGlobal, newMinimum.value, newFood, newEnemy, rad, newAgents, newMean, newStd)
    }.take(max)
  }



}
