package cpscala.TSolver.Model.Heuristic

import cpscala.TSolver.Model.Variable.SerialVar

import scala.reflect.ClassTag


abstract class Heuristic[VT <: SerialVar :ClassTag]() {

  def selectLiteral(level: Int, levelvdense: Array[Int]): (VT, Int)

//  def onFail(): Unit

}
