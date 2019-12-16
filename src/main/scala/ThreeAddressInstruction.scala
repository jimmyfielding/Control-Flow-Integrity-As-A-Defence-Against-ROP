/**
  * Created by Jim on 16/10/2017.
  */

sealed trait ThreeAddressInstruction
case class FunctionLabel(var name: String, var noOfArgs: Int) extends ThreeAddressInstruction {
  def getName: String = {
    name
  }
}
case class FunctionExit(var noOfArgs: Int) extends ThreeAddressInstruction
case class Label(var name: String) extends ThreeAddressInstruction {
  def getName: String = {
    name
  }
}
case class BinaryArithmeticTAC(var op: Op ,var arg1: String ,var arg2: String ) extends ThreeAddressInstruction {
  def getOp: Op = {
    op
  }
  def getArg1: String = {
    arg1
  }
  def getArg2: String = {
    arg2
  }
}
case class CopyTAC(var arg1: String ,var arg2: String) extends ThreeAddressInstruction {
  def getArg1: String = {
    arg1
  }
  def getArg2: String = {
    arg2
  }
}
case class GoToTAC(var label: String) extends ThreeAddressInstruction {
  def setLabel(l: String): Unit = {
    label = l
  }

  def getLabel: String = {
    label
  }
}
case class IfGoToTAC(var cond: Cond,var arg1: String,var arg2: String, var tLabel: String) extends ThreeAddressInstruction {
  def setTLabel(l: String): Unit = {
    tLabel = l
  }

  def getTLabel: String = {
    tLabel
  }
  def getArg1: String = {
    arg1
  }
  def getArg2: String = {
    arg2
  }
  def getCond: Cond = {
    cond
  }
}
case class PutParamTAC(var param: String) extends ThreeAddressInstruction
case class CallTAC(var name: String, var noOfParam: Int) extends ThreeAddressInstruction {
  def getName: String = {
    name
  }
}

sealed trait Op extends ThreeAddressInstruction
case class Plus() extends Op
case class Minus() extends Op
case class Times() extends Op
case class Div() extends Op

sealed trait Cond extends ThreeAddressInstruction
case class LessThan() extends Cond
case class LessThanEq() extends Cond
case class EqualsCond() extends Cond
case class GreaterThanEq() extends Cond
case class GreaterThan() extends Cond
