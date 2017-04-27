package model

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * Created by Michał on 27.04.2017.
  */
class FileLogic {

  private val FILE_PREFIX = System.getProperty("user.dir")+"\\src\\main\\assets\\"


  /**get rotation matrix
    * @return rotation matrix for filename
    */
  def getMr(filename: String) : DenseMatrix[Double] = {
    breeze.linalg.csvread(new File(FILE_PREFIX+filename), ',')
  }

  /**get shift vector
    * @return shift vector for filename
    */
  def getOs(filename: String) : DenseVector[Double] = {
    breeze.linalg.csvread(new File(FILE_PREFIX+filename), ',').toDenseVector
  }

  /**get bent cigar matrix
    * @return rotation matrix for bent cigar function
    */
  def getBentCigarMr : DenseMatrix[Double] = {
    getMr("MrBentcigar.txt")
  }

  /**get rosenbrock matrix
    * @return rotation matrix for rosenbrock function
    */
  def getRosenbrockMr : DenseMatrix[Double] = {
    getMr("MrRosenbrock.txt")
  }

  /**get rastrigin matrix
    * @return rotation matrix for rastrigin function
    */
  def getRastriginMr : DenseMatrix[Double] = {
    getMr("MrRastrigin.txt")
  }

  /**get zakharov matrix
    * @return rotation matrix for zakharov function
    */
  def getZakharovMr : DenseMatrix[Double] = {
    getMr("MrZakharov.txt")
  }

  /**get bentcigar vector
    * @return shift vector for bentcigar function
    */
  def getBentCigarOs : DenseVector[Double] = {
    getOs("OsBentcigar.txt")
  }

  /**get rosenbrock vector
    * @return shift vector for rosenbrock function
    */
  def getRosenbrockOs : DenseVector[Double] = {
    getOs("OsRosenbrock.txt")
  }

  /**get rastrigin vector
    * @return shift vector for rastrigin function
    */
  def getRastriginOs : DenseVector[Double] = {
    getOs("OsRastrigin.txt")
  }

  /**get zakharov vector
    * @return shift vector for zakharov function
    */
  def getZakharovOs : DenseVector[Double] = {
    getOs("OsZakharov.txt")
  }
}
