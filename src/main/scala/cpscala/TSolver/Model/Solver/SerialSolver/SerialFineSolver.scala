package cpscala.TSolver.Model.Solver.SerialSolver

import cpscala.TSolver.Model.Variable.SerialVar
import cpscala.XModel.XModel

/**
  * 细粒度求解器，适用于STR3和STRbit。
  */

class SerialFineSolver(xm: XModel, propagator_name: String, var_type: String, heu_name: String) extends SerialSolver(xm, propagator_name, var_type, heu_name) {

  override def initialPropagate(): Boolean = {

    // 表约束初始化
    for (c <- tabs) {
      c.setup()
    }

    helper.globalStamp += 1
    // 初始删值
    for (c <- tabs) {
      if(!c.setup()){
        return false
      }
    }

    start_time = System.nanoTime
    prop_start_time = System.nanoTime
    // 初始传播
    Q.clear()
    var i = 0
    for (i <- 0 until numVars) {
      if (helper.varStamp(i) != 0) {
        insert(vars(i))
      }
    }

    while (!Q.empty()) {
      val v = Q.pop()
      for (c <- subscription(v.id)) {
        if (helper.varStamp(v.id) > helper.tabStamp(c.id)) {
          Y_evt.clear()
          val consistent = c.propagate(Y_evt)
          helper.c_sum += 1
          if (!consistent) {
            return false
          } else {
            for (y <- Y_evt) {
              insert(y)
            }
          }
          helper.globalStamp += 1
          helper.tabStamp(c.id) = helper.globalStamp
        }
      }
    }
    return true
  }

  override def checkConsistencyAfterAssignment(x: SerialVar): Boolean = {

    Q.clear()
    insert(x)
    while (!Q.empty()) {
      val v = Q.pop()
      for (c <- subscription(v.id)) {
        if (helper.varStamp(v.id) > helper.tabStamp(c.id)) {
          Y_evt.clear()
          val consistent = c.propagate(Y_evt)
          helper.c_sum += 1
          if (!consistent) {
            return false
          } else {
            for (y <- Y_evt) {
              insert(y)
            }
          }
          helper.globalStamp += 1
          helper.tabStamp(c.id) = helper.globalStamp
        }
      }
    }
    return true
  }

  override def checkConsistencyAfterRefutation(x: SerialVar): Boolean = {

    Q.clear()
    insert(x)
    while (!Q.empty()) {
      val v = Q.pop()
      for (c <- subscription(v.id)) {
        if (helper.varStamp(v.id) > helper.tabStamp(c.id)) {
          Y_evt.clear()
          val consistent = c.propagate(Y_evt)
          helper.c_sum += 1
          if (!consistent) {
            return false
          } else {
            for (y <- Y_evt) {
              insert(y)
            }
          }
          helper.globalStamp += 1
          helper.tabStamp(c.id) = helper.globalStamp
        }
      }
    }
    return true
  }

  def insert(x: SerialVar): Unit = {
    Q.push(x)
    helper.globalStamp += 1
    helper.varStamp(x.id) = helper.globalStamp
  }

}
