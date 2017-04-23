package da

import breeze.linalg._
import breeze.numerics.{abs, pow, sin}

/**
  * Created by wojciech on 22.04.17. 
  */
object Tools {

  type V = DenseVector[Double]

  def inRange(radius: V, v1: V, v2: V): Boolean ={
    val dist = norm(v1 - v2)
    norm(radius) > dist && dist > 10e-10
  }

  // 3.1
  def separation(neighboursPos: List[V], agentPos: V): V = neighboursPos
    .map(_ - agentPos)
    .reduceOption(_ + _)
    .getOrElse(DenseVector.zeros(agentPos.length))

  // 3.2
  def alignment(neighboursVelocity: List[V], agentVelocity: V): V = neighboursVelocity
    .reduceOption(_ + _)
    .map(_ *:* (1.0/neighboursVelocity.size))
    .getOrElse(agentVelocity) // zeros???

  // 3.3
  def cohesion(neighboursPos: List[V], agentPos: V): V = neighboursPos
    .reduceOption(_ + _)
    .map(_ *:* (1.0/neighboursPos.size))
    .map(_ - agentPos)
    .getOrElse(DenseVector.zeros(agentPos.length))

  // 3.4
  def food(radius: V, foodPos: V, agentPos: V): V =
    if(inRange(radius, foodPos, agentPos)) foodPos - agentPos
    else DenseVector.zeros(agentPos.length)

  // 3.5
  def enemy(radius: V, enemyPos: V, agentPos: V): V =
    if(inRange(radius, enemyPos, agentPos)) enemyPos + agentPos
    else DenseVector.zeros(agentPos.length)

  // 3.6
  def updateVelocity(radius: V, neighboursV: List[V], neighboursP: List[V], foodP: V, enemyP: V, agentP: V, agentV: V, p: Parameters): V = {
    val alignment = Tools.alignment(neighboursV, agentV)
    val cohesion = Tools.cohesion(neighboursP, agentP)
    val separation = Tools.separation(neighboursP, agentP)
    val food = Tools.food(radius, foodP, agentP)
    val enemy = Tools.enemy(radius, enemyP, agentP)
    (agentV *:* p.w) + (alignment *:* p.a) + (cohesion *:* p.c) + (separation *:* p.s) + (food *:* p.f) + (enemy *:* p.e)
  }

  // 3.7
  def updatePosition(agentPos: V, agentVelocity: V): V =
    agentPos + agentVelocity

  // 3.8
  def updateLevy(agentPos: V): V =
    agentPos + (agentPos *:* levy(agentPos.length))

  // 3.9
  private def levy(dim: Int): V = {
    val r1 = DenseVector.fill(dim, math.random())
    val r2 = DenseVector.fill(dim, math.random())
    0.01 * ((r1 *:* omega) / pow(abs(r2), 1.0/beta))
  }

  // 3.10
  private val beta = 1.5
  private val omega: Double = {
    val gamma1d25 = 1.1330030963193471
    val gamma2d5 = 3.323350970447843
    val l = gamma2d5 * sin(math.Pi*beta/2.0)
    val m = gamma1d25 * beta * pow(2.0, (beta - 1.0)/2.0)
    pow(l/m, 1/beta)
  }


}
