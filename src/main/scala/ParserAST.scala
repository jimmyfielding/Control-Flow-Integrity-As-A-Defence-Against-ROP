/**
  * Created by Jim on 13/10/2017.
  */

sealed trait ParserAST
case class ProgramAST(var decls: List[DeclarationAST]) extends ParserAST {

  	 assert(decls.nonEmpty)
		 assert(decls.head.numOfArgs == 0)

}
case class DeclarationAST(var id: String, var numOfArgs: Int, var body: BlockAST) extends ParserAST
case class BlockAST(var expressionList: List[ExpressionAST]) extends ParserAST
case class ParserErrorAST(var msg: String) extends ParserAST

sealed trait ExpressionAST extends ParserAST
case class IfAST(var l: ExpressionAST, var comp: CompAST, var r: ExpressionAST, var thenBody: ExpressionAST, var elseBody: ExpressionAST) extends ExpressionAST
case class WhileAST(var l: ExpressionAST, var comp: CompAST, var r: ExpressionAST, var body: BlockAST) extends ExpressionAST
case class RepeatUntilAST(var body: ExpressionAST, var l: ExpressionAST, var comp: CompAST, var r: ExpressionAST) extends ExpressionAST
case class BinExpAST(var l: ExpressionAST, var binop: BinopAST, var r: ExpressionAST) extends ExpressionAST
case class AssignAST(var x: Int, var e: ExpressionAST) extends ExpressionAST
case class VariableAST(var x: Int) extends ExpressionAST {
  def getOffset: Int = {
    x
  }
}
case class ParameterAST(var s: String)
case class SeqAST(var l: ExpressionAST, var r: ExpressionAST) extends ExpressionAST
case class InvokeAST(var name: String, var args: List[ParameterAST]) extends ExpressionAST
case class IntLiteralAST(var s: String) extends ExpressionAST {
  def getContents: String = {
    s
  }
}
case class IdentifierAST(var s: String) extends ExpressionAST


sealed trait BinopAST extends ParserAST
case class PlusAST extends BinopAST
case class TimesAST extends BinopAST
case class MinusAST extends BinopAST
case class DivAST extends BinopAST


sealed trait CompAST extends ParserAST
case class LessThanAST extends CompAST
case class LessThanEqAST extends CompAST
case class EqualCompAST extends CompAST
case class GreaterThanEqAST extends CompAST
case class GreaterThanAST extends CompAST
