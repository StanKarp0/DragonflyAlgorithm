package model

import da.Parameters

/**
  * Created by wojciech on 22.04.17. 
  */
case class ConstParam(
                     override val a: Double,
                     override val c: Double,
                     override val e: Double,
                     override val f: Double,
                     override val s: Double,
                     override val w: Double
                     ) extends Parameters
