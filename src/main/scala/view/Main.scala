package view

import breeze.linalg._
import model.{ConstParam, DA, Func, VariableParam}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
/**
  * Created by wojciech on 22.04.17. 
  */
object Main extends JFXApp {

  val D = 2 // dimensions, nx

  val lb = DenseVector.fill[Double](D, -100)
  val ub = DenseVector.fill[Double](D, 100)

  val func = new Func();

  //val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1) + 1.0
  //val fit = (x: DenseVector[Double]) => func.bentCigarFunc(x, sFlag = true, rFlag = true)
  val fit = (x: DenseVector[Double]) => x(0) * x(0) + x(1) * x(1)// + x(2) * x(2)+ x(3) * x(3)+ x(4) * x(4)+ x(5) * x(5)+ x(6) * x(6)+ x(7) * x(7)+ x(8) * x(8)+ x(9) * x(9)

  val da = new DA(fit, 100, lb, ub, VariableParam)
  val result = da.iterator(1000).take(1000).toList
  println(result.last.global)

  def v2xy(x: DenseVector[Double]): XY = XY(x(0), x(1))
  val history = result.map(_.agents.map(_.x).map(v2xy)).transpose.map(_.reverse)


  stage = new PrimaryStage {

    scene = new Scene(1000, 700) {

      root = new BorderPane() {
        style = "-fx-background-color: white;"
        val scale: (XY) => XY = (v: XY) => (v * 2) + XY(500, 350)
        val interval = 0.5
        val food = Particle(result.map(_.food).map(v2xy).reverse, scale, interval)
        food.fill = Color.Green
        food.radius = 1.5
        val enemy = Particle(result.map(_.enemy).map(v2xy).reverse, scale, interval)
        enemy.fill = Color.Red
        enemy.radius = 1.5
        val particles: List[Particle] = history.map{ l => Particle(l, scale, interval)}
        center = new Pane {
          children = particles :+ enemy :+ food
        }
        top = new Button("Start") {
          onAction = (_: ActionEvent) => {

            particles.foreach{p =>
              p.timeline.play()
            }
            food.timeline.play()
            enemy.timeline.play()
          }

        }

      }

    }

  }

}
