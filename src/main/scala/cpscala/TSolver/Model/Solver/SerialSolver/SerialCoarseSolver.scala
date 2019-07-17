package cpscala.TSolver.Model.Solver.SerialSolver

import cpscala.TSolver.Model.Variable.SerialVar
import cpscala.XModel.XModel

/**
  *粗粒度求解器，适用于STR2。
  */

class SerialCoarseSolver(xm: XModel, propagatorName: String, varType: String, heuName: String) extends SerialSolver(xm, propagatorName, varType, heuName) {

  override def initialPropagate(): Boolean = {
    start_time = System.nanoTime
    prop_start_time = System.nanoTime
    return propagate(null)
  }

  override def checkConsistencyAfterAssignment(x: SerialVar): Boolean = {
    return propagate(x)
  }

  override def checkConsistencyAfterRefutation(x: SerialVar): Boolean = {
    return propagate(x)
  }

  def propagate(x: SerialVar): Boolean = {
    Q.clear()
    if (x == null) {
      //初始化
      for (z <- vars) {
        insert(z)
      }
    } else {
      insert(x)
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
      helper.p_sum += 1
    }

    return true
  }

    def insert(x: SerialVar): Unit = {
    Q.push(x)
    helper.globalStamp += 1
    helper.varStamp(x.id) = helper.globalStamp
  }

}
