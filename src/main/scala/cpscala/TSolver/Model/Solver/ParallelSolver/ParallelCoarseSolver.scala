package cpscala.TSolver.Model.Solver.ParallelSolver

import cpscala.TSolver.CpUtil.Constants
import cpscala.TSolver.Model.Variable.ParallelVar
import cpscala.XModel.XModel

class ParallelCoarseSolver(xm: XModel, parallelism: Int, propagatorName: String, varType: String, heuName: String) extends ParallelSolver(xm, parallelism, propagatorName, varType, heuName) {

  private[this] val subMask = Array.fill[Long](numBitTabs)(-1L)

  def initialPropagate(): Boolean = {

    start_time = System.nanoTime
    prop_start_time = System.nanoTime

    helper.isConsistent = true

    do {
      helper.getTableMask(subMask)
      helper.clearTableMask()
      helper.varIsChange = false
      //      helper.c_sum = 0

      // 提交约束至线程池
      var i = 0
      var base = 0
      var j = 0
      var end = 0
      var cid = 0
      while (i < subMask.length && helper.isConsistent) {
        val a = subMask(i)
        if (a != 0) {
          base = i * Constants.BITSIZE
          j = Constants.FirstLeft(a)
          end = Constants.FirstRight(a)
          while (j <= end) {
            if ((a & Constants.MASK1(j)) != 0) {
              cid = j + base
              helper.c_sum += 1
              helper.submitToPool(tabs(cid))
            }
            j += 1
          }
        }
        i += 1
      }

      helper.poolAwait()
      helper.p_sum += 1

      if (!helper.isConsistent) {
        return false
      }

    } while (helper.varIsChange)

    return true
  }

  def checkConsistencyAfterAssignment(ix: ParallelVar): Boolean = {

    helper.clearTableMask()
    helper.addToTableMask(ix.id)
    helper.isConsistent = true

    do {
      helper.getTableMask(subMask)
      helper.clearTableMask()
      helper.varIsChange = false

      // 提交约束至线程池
      var i = 0
      var base = 0
      var j = 0
      var end = 0
      var cid = 0
      while (i < subMask.length && helper.isConsistent) {
        val a = subMask(i)
        if (a != 0) {
          base = i * Constants.BITSIZE
          j = Constants.FirstLeft(a)
          end = Constants.FirstRight(a)
          while (j <= end) {
            if ((a & Constants.MASK1(j)) != 0) {
              cid = j + base
              helper.c_sum += 1
              helper.submitToPool(tabs(cid))
            }
            j += 1
          }
        }
        i += 1
      }
      helper.poolAwait()
      helper.p_sum += 1

      if (!helper.isConsistent) {
        return false
      }

    } while (helper.varIsChange)

    return true
  }

  def checkConsistencyAfterRefutation(ix: ParallelVar): Boolean = {

    helper.clearTableMask()
    helper.addToTableMask(ix.id)
    helper.isConsistent = true

    do {
      helper.getTableMask(subMask)
      helper.clearTableMask()
      helper.varIsChange = false

      // 提交约束至线程池
      var i = 0
      var base = 0
      var j = 0
      var end = 0
      var cid = 0
      while (i < subMask.length && helper.isConsistent) {
        val a = subMask(i)
        if (a != 0) {
          base = i * Constants.BITSIZE
          j = Constants.FirstLeft(a)
          end = Constants.FirstRight(a)
          while (j <= end) {
            if ((a & Constants.MASK1(j)) != 0) {
              cid = j + base
              helper.c_sum += 1
              helper.submitToPool(tabs(cid))
            }
            j += 1
          }
        }
        i += 1
      }
      helper.poolAwait()
      helper.p_sum += 1

      if (!helper.isConsistent) {
        return false
      }

    } while (helper.varIsChange)

    return true
  }

}

