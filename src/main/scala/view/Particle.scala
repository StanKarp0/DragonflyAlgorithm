package view

import scalafx.Includes._
import scalafx.animation.Timeline
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

/**
  * Created by wojciech on 22.04.17.
  */
case class XY(x: Double, y: Double) {
  def *(d: Double) = XY(x * d, y * d)
  def +(v: XY): XY = XY(x + v.x, y + v.y)
}

abstract class Particle extends Circle {
  val timeline: Timeline
}

object Particle {

  def apply(history: List[XY], scale: XY => XY, interval: Double): Particle = {

    new Particle() {
      history.lastOption.foreach{v =>
        val sv = scale(v)
        centerX = sv.x
        centerY = sv.y
      }
      radius = 1
      fill = Color.Black
      val timeline = new Timeline {
        this.autoReverse = false
        keyFrames = history.reverse.map(scale).zipWithIndex.flatMap { case (v, i) => Seq(
          at((i * interval).s) {
            centerX -> v.x
          },
          at((i * interval).s) {
            centerY -> v.y
          }
        )
        }
      }
    }
  }

}
