package model

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.{cos, pow}

/**
  * Created by Michał on 27.04.2017.
  */
class Func {

  val D = 10 // dimensions, nx
  val fileLogic: FileLogic = new FileLogic()
  val bcMr: DenseMatrix[Double] = fileLogic.getBentCigarMr
  val roMr: DenseMatrix[Double] = fileLogic.getRosenbrockMr
  val raMr: DenseMatrix[Double] = fileLogic.getRastriginMr
  val zaMr: DenseMatrix[Double] = fileLogic.getZakharovMr
  val bcOs: DenseVector[Double] = fileLogic.getBentCigarOs
  val roOs: DenseVector[Double] = fileLogic.getRosenbrockOs
  val raOs: DenseVector[Double] = fileLogic.getRastriginOs
  val zaOs: DenseVector[Double] = fileLogic.getZakharovOs

  /**
    * shift function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @return polozenie po przesunieciu
    */
  def shiftFunc(x : DenseVector[Double], Os : DenseVector[Double]) : DenseVector[Double] = {
    val xshift = DenseVector.zeros[Double](D)
    for(i <- 0 until D){
      xshift(i) = x(i) - Os(i)
    }
    xshift
  }

  /**
    * rotate function
    * @param x - oryginalne polozenie
    * @param Mr - macierz obrotu
    * @return - polozenie po obrocie
    */
  def rotateFunc(x : DenseVector[Double], Mr : DenseMatrix[Double]) : DenseVector[Double] = {
    val xrot = DenseVector.zeros[Double](D)
    for(i <- 0 until D){
      for(j <- 0 until D){
        xrot(i) = xrot(i) + x(j)*Mr(i, j)
      }
    }
    xrot
  }

  /**
    * shift and rotate function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @param Mr - macierz obrotu
    * @param shRate - wspolczynnik zmniejszenia wyniku
    * @param sFlag - czy przesuwac
    * @param rFlag - czy rotowac
    * @return przesuniete i zrotowane polozenie
    */
  def srFunc(x: DenseVector[Double], Os: DenseVector[Double], Mr: DenseMatrix[Double], shRate: Double,
            sFlag: Boolean, rFlag: Boolean): DenseVector[Double] ={
    var srX = DenseVector.zeros[Double](D)
    if(sFlag){
      if(rFlag){
        val y = shiftFunc(x, Os)
        for(i <- 0 until D){
          y(i) = y(i) * shRate
        }
        srX = rotateFunc(y, Mr)
      } else{
        srX = shiftFunc(x, Os)
        for(i <- 0 until D){
          srX(i) = srX(i) * shRate
        }
      }
    } else{
      if(rFlag){
        val y = DenseVector.zeros[Double](D)
        for(i <- 0 until D){
          y(i) = x(i) * shRate
        }
        srX = rotateFunc(y, Mr)
      } else{
        for(i <- 0 until D){
          srX(i) = x(i) * shRate
        }
      }
    }
    srX
  }

  /**
    * bent cigar function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @param Mr - macierz obrotu
    * @param sFlag - czy przesuwac
    * @param rFlag - czy rotowac
    * @return wynik
    */
  def bentCigarFunc(x: DenseVector[Double], sFlag: Boolean, rFlag: Boolean,
                    Os: DenseVector[Double] = bcOs, Mr: DenseMatrix[Double] = bcMr) : Double = {
    val z = srFunc(x, Os, Mr, 1.0, sFlag, rFlag)
    var f = z(0)*z(0)
    for(i <- 1 until D){
      f = f + pow(10, 6)*z(i)*z(i)
    }
    f
  }

  /**
    * bent cigar function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @param Mr - macierz obrotu
    * @param sFlag - czy przesuwac
    * @param rFlag - czy rotowac
    * @return wynik
    */
  def rosenbrockFunc(x: DenseVector[Double], sFlag: Boolean, rFlag: Boolean,
                     Os: DenseVector[Double] = roOs, Mr: DenseMatrix[Double] = roMr) : Double = {
    var tmp1, tmp2: Double = 0
    var f: Double = 0
    val z = srFunc(x, Os, Mr, 2048.0/100.0, sFlag, rFlag)
    z(0) = z(0)+1 //shift to origin
    for(i <- 0 until D-1){
      z(i+1) = z(i+1) + 1 //shift to origin
      tmp1 = z(i)*z(i) - z(i+1)
      tmp2 = z(i) - 1
      f = f + 100*tmp1*tmp1 + tmp2*tmp2
    }
    f
  }

  /**
    * rastrigin function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @param Mr - macierz obrotu
    * @param sFlag - czy przesuwac
    * @param rFlag - czy rotowac
    * @return wynik
    */
  def rastriginFunc(x: DenseVector[Double], sFlag: Boolean, rFlag: Boolean,
                    Os: DenseVector[Double] = raOs, Mr: DenseMatrix[Double] = raMr) : Double = {
    var f: Double = 0
    val z = srFunc(x, Os, Mr, 5.12/100.0, sFlag, rFlag)
    for(i <- 0 until D){
      f = f + (z(i)*z(i) - 10.0*cos(2.0*scala.math.Pi*z(i)) + 10)
    }
    f
  }

  /**
    * zakharov function
    * @param x - oryginalne polozenie
    * @param Os - wektor przesuniecia
    * @param Mr - macierz obrotu
    * @param sFlag - czy przesuwac
    * @param rFlag - czy rotowac
    * @return wynik
    */
  def zakharovFunc(x: DenseVector[Double], sFlag: Boolean, rFlag: Boolean,
                   Os: DenseVector[Double] = zaOs, Mr: DenseMatrix[Double] = zaMr) : Double = {
    val z = srFunc(x, Os, Mr, 1.0, sFlag, rFlag)
    var f: Double = 0
    var sum1, sum2: Double = 0
    for(i <- 0 until D){
      val xi: Double = z(i)
      sum1 = sum1 + pow(xi, 2)
      sum2 = sum2 + 0.5*(i+1)*xi
    }
    f = sum1 + pow(sum2, 2) + pow(sum2, 4)
    f
  }

}
