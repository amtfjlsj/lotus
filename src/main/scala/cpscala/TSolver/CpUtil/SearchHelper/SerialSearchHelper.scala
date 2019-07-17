package cpscala.TSolver.CpUtil.SearchHelper

class SerialSearchHelper(val numVars: Int, val numTabs: Int) {
  // 初始化各时间戳
  var globalStamp: Long = 0L
  val tabStamp: Array[Long] = Array.fill(numTabs)(0L)
  val varStamp: Array[Long] = Array.fill(numVars)(0L)

  // 搜索时间
  var time: Long = 0L
  var branchTime = 0L
  var propTime = 0L
  var backTime = 0L
  var nodes: Long = 0L
  var isConsistent: Boolean = true
  // 搜索上限
  var timeLimit = 0L

  var level: Int = 0

  //线程启动次数
  var p_sum = 0L
  //约束传播次数
  var c_sum = 0L
}
