package cpscala.TSolver.Model.Solver.SerialSolver

import cpscala.TSolver.CpUtil.SearchHelper.SerialSearchHelper
import cpscala.TSolver.CpUtil.{AssignedStack, CoarseQueue}
import cpscala.TSolver.Model.Constraint.SerialConstraint._
import cpscala.TSolver.Model.Heuristic.{HeuDomDdeg, HeuDomWdeg, Heuristic}
import cpscala.TSolver.Model.Variable._
import cpscala.XModel.{XModel, XTab, XVar}

import scala.collection.mutable.ArrayBuffer

abstract class SerialSolver(xm: XModel, propagatorName: String, varType: String, heuName: String) {
  val numVars: Int = xm.num_vars
  val numTabs: Int = xm.num_tabs
  val vars = new Array[SerialVar](numVars)
  val tabs = new Array[SerialPropagator[SerialVar]](numTabs)
  val helper = new SerialSearchHelper(numVars, numTabs)

  //记录已赋值的变量
  val levelvsparse = Array.range(0, numVars)
  val levelvdense = Array.range(0, numVars)

  val subscription = new Array[ArrayBuffer[SerialPropagator[SerialVar]]](numVars)
  for (i <- 0 until numVars) {
    subscription(i) = new ArrayBuffer[SerialPropagator[SerialVar]]()
  }

  // 启发式对象
  var heuristic: Heuristic[SerialVar] = null

  // 初始化变量
  varType match {
    case "BitSet" => {
      for (i <- 0 until numVars) {
        val xv: XVar = xm.vars.get(i)
        vars(i) = new BitSetSerialVar(xv.name, xv.id, numVars, xv.values, helper)
      }
    }
  }

  //初始化约束
  propagatorName match {
    case "STRbit" => {
      for (i <- 0 until numTabs) {
        val xc: XTab = xm.tabs.get(i)
        val ts: Array[Array[Int]] = xc.tuples
        val scope: Array[SerialVar] = for (i <- (0 until xc.arity).toArray) yield vars(xc.scopeInt(i))
        tabs(i) = new STRbit(xc.id, xc.arity, numVars, scope, ts, helper)

        for (v <- scope) {
          subscription(v.id) += tabs(i)
        }
      }
    }

    case "CT" => {
      for (i <- 0 until numTabs) {
        val xc: XTab = xm.tabs.get(i)
        val ts: Array[Array[Int]] = xc.tuples
        val scope: Array[SerialVar] = for (i <- (0 until xc.arity).toArray) yield vars(xc.scopeInt(i))
        tabs(i) = new CT(xc.id, xc.arity, numVars, scope, ts, helper)

        for (v <- scope) {
          subscription(v.id) += tabs(i)
        }
      }
    }
  }

  // 初始化启发式类
  heuName match {
    case "Dom/Ddeg" => {
      heuristic = new HeuDomDdeg[SerialVar, SerialPropagator[SerialVar]](numVars, vars, subscription)
    }

    case "Dom/Wdeg" => {
      heuristic = new HeuDomWdeg[SerialVar, SerialPropagator[SerialVar]](numVars, vars, subscription)
    }
  }

  val Q = new CoarseQueue[SerialVar](numVars)
  var Y_evt: ArrayBuffer[SerialVar] = new ArrayBuffer[SerialVar](xm.max_arity)

  val I = new AssignedStack[SerialVar](xm.num_vars)

  var start_time = 0L
  var branch_start_time = 0L
  var prop_start_time = 0L
  var back_start_time = 0L
  var end_time = 0L

  def search(timeLimit: Long): Unit = {
    var finished = false

    //initial propagate
    var consistent = initialPropagate()
    end_time = System.nanoTime
    helper.propTime += (end_time - prop_start_time)

    if (!consistent) {
      finished = false
      end_time = System.nanoTime
      helper.time = end_time - start_time
      return
    }

    while (!finished) {
      end_time = System.nanoTime
      helper.time = end_time - start_time
      if (helper.time > timeLimit) {
        return
      }

      branch_start_time = System.nanoTime
      val (v, a) = heuristic.selectLiteral(helper.level, levelvdense)
      newLevel()
      helper.nodes += 1
      I.push(v, a)
      bind(v, a)
      end_time = System.nanoTime
      helper.branchTime += (end_time - branch_start_time)


      prop_start_time = System.nanoTime
      consistent = checkConsistencyAfterAssignment(v)
      end_time = System.nanoTime
      helper.propTime += (end_time - prop_start_time)

      if (consistent && I.full()) {
        I.show()
        //         若想求出所有解，则将consistent设置为false，且不返回
        //        consistent = false
        end_time = System.nanoTime
        helper.time = end_time - start_time
        return
      }

      while (!consistent && !I.empty()) {
        back_start_time = System.nanoTime
        val (v, a) = I.pop()
        backLevel()
        v.remove(a)
        remove(v, a)
        end_time = System.nanoTime
        helper.backTime += (end_time - back_start_time)

        prop_start_time = System.nanoTime
        consistent = !v.isEmpty() && checkConsistencyAfterRefutation(v)
        end_time = System.nanoTime
        helper.propTime += (end_time - prop_start_time)
      }

      if (!consistent) {
        finished = true
      }
    }
    end_time = System.nanoTime
    helper.time = end_time - start_time
    return
  }

  def initialPropagate(): Boolean

  def checkConsistencyAfterAssignment(ix: SerialVar): Boolean

  def checkConsistencyAfterRefutation(ix: SerialVar): Boolean

  def newLevel(): Unit = {
    helper.level += 1
    for (v <- vars) {
      v.newLevel()
    }

    for (c <- tabs) {
      c.newLevel()
    }
  }

  def backLevel(): Unit = {
    helper.level -= 1
    for (v <- vars) {
      v.backLevel()
    }
    for (c <- tabs) {
      c.backLevel()
    }
  }

  def remove(v: SerialVar, a: Int): Unit = {
    //约束的已实例化变量个数减1
    for (c <- subscription(v.id)) {
      c.assignedCount -= 1
    }
    v.remove(a)
    helper.globalStamp += 1
    helper.varStamp(v.id) = helper.globalStamp
  }

  def bind(v: SerialVar, a: Int): Unit = {
    //在稀疏集上交换变量
    val minvi = levelvsparse(v.id)
    val vid = levelvdense(helper.level - 1)
    levelvdense(helper.level - 1) = levelvdense(minvi)

    levelvsparse(vid) = minvi
    levelvsparse(levelvdense(minvi)) = helper.level - 1

    levelvdense(minvi) = vid

    for (c <- subscription(v.id)) {
      c.assignedCount += 1
    }
    v.bind(a)
    helper.globalStamp += 1
    helper.varStamp(v.id) = helper.globalStamp
  }

}
