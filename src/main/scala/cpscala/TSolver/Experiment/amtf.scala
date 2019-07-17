package cpscala.TSolver.Experiment

import cpscala.TSolver.CpUtil.Constants
import cpscala.TSolver.Model.Solver.ParallelSolver._
import cpscala.TSolver.Model.Solver.SerialSolver._
import cpscala.XModel.XModel

import scala.xml.XML

object amtf {
  def main(args: Array[String]): Unit = {

    val xf = XML.loadFile("benchmarks/amtfPath.xml")
    val fileNode = xf \\ "BMFile"
    val path = fileNode.text
    val fmt = (fileNode \\ "@format").text.toInt
    println(path)
    val xm = new XModel(path, true, fmt)

    var i = 0
    var parallelism = 1
    var node = 0L
    var time = 0L
    var branchTime = 0L
    var propTime = 0L
    var backTime = 0L
    var pType = " "
    var ppType = " "
    var varType = " "
    var heuName = " "
    var exe = 1
    var p_sum = 0L
    var c_sum = 0L
    val maxPara = 3

//    time = 0L
//    branchTime = 0L
//    propTime = 0L
//    backTime = 0L
//    pType = "STRbit"
//    varType = "BitSet"
//    heuName = "Dom/Ddeg"
//    println(s"${pType} ${heuName}===============>")
//    i = 0
//    while (i < exe) {
//      val strbit = new SerialFineSolver(xm, pType, varType, heuName)
//      strbit.search(Constants.TIME)
//      node = strbit.helper.nodes
//      time += strbit.helper.time
//      branchTime += strbit.helper.branchTime
//      propTime += strbit.helper.propTime
//      backTime += strbit.helper.backTime
//      c_sum = strbit.helper.c_sum
//      i += 1
//    }
//    println("node = " + node)
//    println("search time = " + (time / exe).toDouble * 1e-9 + "s")
//    println("branch time = " + (branchTime / exe).toDouble * 1e-9 + "s")
//    println("propagate time = " + (propTime / exe).toDouble * 1e-9 + "s")
//    println("backtrack time = " + (backTime / exe).toDouble * 1e-9 + "s")
//    println("c_sum = " + c_sum)
//    //
//    //
//    ppType = "PSTRbit"
//    varType = "SafeBitSet"
//    heuName = "Dom/Ddeg"
//    parallelism = 1
//    while (parallelism <= maxPara) {
//      time = 0L
//      branchTime = 0L
//      propTime = 0L
//      backTime = 0L
//      c_sum = 0L
//      println(s"${parallelism}线程 ${ppType} ${heuName}===============>")
//      for (i <- 1 to exe) {
//        val pstrbit = new ParallelFineSolver(xm, parallelism, ppType, varType, heuName)
//        pstrbit.search(Constants.TIME)
//        pstrbit.shutdown()
//        node = pstrbit.helper.nodes
//        time += pstrbit.helper.time
//        branchTime += pstrbit.helper.branchTime
//        propTime += pstrbit.helper.propTime
//        backTime += pstrbit.helper.backTime
//        p_sum = pstrbit.helper.p_sum
//        c_sum += pstrbit.helper.c_sum
//      }
//      println("node = " + node)
//      println("search time = " + (time / exe).toDouble * 1e-9 + "s")
//      println("branch time = " + (branchTime / exe).toDouble * 1e-9 + "s")
//      println("propagate time = " + (propTime.toDouble / exe * 1e-9).formatted("%.2f") + "s")
//      println("backtrack time = " + (backTime / exe).toDouble * 1e-9 + "s")
//      println("p_sum = " + p_sum)
//      println("c_sum = " + c_sum / exe)
//      parallelism += 1
//    }
//
//
    time = 0L
    branchTime = 0L
    backTime = 0L
    propTime = 0L
    pType = "CT"
    varType = "BitSet"
    heuName = "Dom/Wdeg"
    println(s"${pType} ${heuName}===============>")
    i = 0
    while (i < exe) {
      val ct = new SerialCoarseSolver(xm, pType, varType, heuName)
      ct.search(Constants.TIME)
      node = ct.helper.nodes
      time += ct.helper.time
      branchTime += ct.helper.branchTime
      propTime += ct.helper.propTime
      backTime += ct.helper.backTime
      c_sum = ct.helper.c_sum
      i += 1
    }
    println("node = " + node)
    println("search time = " + (time / exe).toDouble * 1e-9 + "s")
    println("branch time = " + (branchTime / exe).toDouble * 1e-9 + "s")
    println("propagate time = " + (propTime.toDouble / exe * 1e-9).formatted("%.2f") + "s")
    println("backtrack time = " + (backTime / exe).toDouble * 1e-9 + "s")
    println("c_sum = " + c_sum)


    ppType = "PCT"
    varType = "SafeBitSet"
    heuName = "Dom/Ddeg"
    parallelism = 6
    while (parallelism <= maxPara) {
      time = 0L
      branchTime = 0L
      propTime = 0L
      backTime = 0L
      c_sum = 0L
      println(s"${parallelism}线程 ${ppType} ${heuName}===============>")
      for (i <- 1 to exe) {
        val pct = new ParallelCoarseSolver(xm, parallelism, ppType, varType, heuName)
        pct.search(Constants.TIME)
        pct.shutdown()
        node = pct.helper.nodes
        time += pct.helper.time
        branchTime += pct.helper.branchTime
        propTime += pct.helper.propTime
        backTime += pct.helper.backTime
        p_sum = pct.helper.p_sum
        c_sum += pct.helper.c_sum
      }
      println("node = " + node)
      println("search time = " + (time / exe).toDouble * 1e-9 + "s")
      println("branch time = " + (branchTime / exe).toDouble * 1e-9 + "s")
      println("propagate time = " + (propTime.toDouble / exe * 1e-9) + "s")
      println("backtrack time = " + (backTime / exe).toDouble * 1e-9 + "s")
      println("p_sum = " + p_sum)
      println("c_sum = " + c_sum / exe)
      parallelism += 1
    }

  }
}
