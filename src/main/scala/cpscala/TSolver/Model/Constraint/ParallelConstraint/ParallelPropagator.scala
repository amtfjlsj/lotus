package cpscala.TSolver.Model.Constraint.ParallelConstraint

import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicLong

import cpscala.TSolver.Model.Constraint.SerialConstraint.SerialPropagator
import cpscala.TSolver.Model.Variable.ParallelVar

import scala.collection.mutable

abstract class ParallelPropagator extends SerialPropagator[ParallelVar] with Callable[Unit] {

  def propagate(): Boolean

}