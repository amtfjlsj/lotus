package cpscala.TSolver.Model.Constraint.SerialConstraint

import cpscala.TSolver.CpUtil.SearchHelper.SerialSearchHelper
import cpscala.TSolver.Model.Variable.SerialVar

import scala.collection.mutable.ArrayBuffer

abstract class SerialPropagator[VT <: SerialVar] {

  val id: Int
  val arity: Int
  val scope: Array[VT]
  var level = 0
  // 约束scope中被赋值的变量个数
  var assignedCount = 0
  // 失败权重，搜索过程中该约束的传播失败次数，在一些启发式中会用到，比如dom/wdeg
  var failWeight = 0
  val helper: SerialSearchHelper

  def setup(): Boolean = ???

  def propagate(evt: ArrayBuffer[SerialVar]): Boolean = ???

  def newLevel(): Unit

  def backLevel(): Unit

  def stepUp(num_vars: Int): Unit = ???

  def isEntailed(): Boolean = ???

  def isSatisfied(): Unit = ???

}
