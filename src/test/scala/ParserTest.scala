import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ParserTest extends FlatSpec with Matchers {

  they should "parse programs correctly" in {
    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_Identifier("a"),T_Assign(),T_Integer("1"),T_RightCurlyBracket())) should equal (ProgramAST
             (List(DeclarationAST("main",0,BlockAST(List(AssignAST(0, IntLiteralAST("1"))))))))

    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_LeftBracket(),T_Integer("1"),T_Plus(),T_Integer("1"),T_RightBracket(),T_RightCurlyBracket())) should equal (ProgramAST
              (List(DeclarationAST("main",0,BlockAST(List(BinExpAST(IntLiteralAST("1"), PlusAST(), IntLiteralAST("1"))))))))

    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_If(), T_Integer("1"), T_LessThan(), T_Integer("2"), T_Then(), T_LeftCurlyBracket(), T_LeftBracket(),T_Integer("1"),T_Plus(),T_Integer("1"),T_RightBracket(),T_RightCurlyBracket(),T_Else(),T_LeftCurlyBracket(), T_LeftBracket(),T_Integer("1"),T_Minus(),T_Integer("1"),T_RightBracket(),T_RightCurlyBracket(),T_RightCurlyBracket())) should equal (ProgramAST
           (List(DeclarationAST("main",0,BlockAST(List(IfAST(IntLiteralAST("1"),LessThanAST(),IntLiteralAST("2"),BinExpAST(IntLiteralAST("1"), PlusAST(), IntLiteralAST("1")),BinExpAST(IntLiteralAST("1"), MinusAST(), IntLiteralAST("1")))))))))

    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_Identifier("a"),T_Assign(),T_Integer("1"),T_Identifier("b"),T_Assign(),T_Identifier("a"),T_RightCurlyBracket())) should equal (ProgramAST
           (List(DeclarationAST("main",0,BlockAST(List(AssignAST(0, IntLiteralAST("1")),AssignAST(0, VariableAST(0))))))))

    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_Identifier("x"),T_Assign(),T_Integer("1"),T_While(),T_Identifier("x"),T_GreaterThan(),T_Integer("1"),T_Do(),T_LeftCurlyBracket(),T_LeftBracket(),T_Integer("1"),T_Plus(),T_Integer("1"),T_RightBracket(),T_RightCurlyBracket(),T_RightCurlyBracket())) should equal (ProgramAST
           (List(DeclarationAST("main",0,BlockAST(List(AssignAST(0, IntLiteralAST("1")),WhileAST(VariableAST(0),GreaterThanAST(),IntLiteralAST("1"),BlockAST(List(BinExpAST(IntLiteralAST("1"), PlusAST(), IntLiteralAST("1")))))))))))

    Parser(List(T_Def(), T_Identifier("main"),
           T_LeftBracket(),T_RightBracket(),T_Equal(),
           T_LeftCurlyBracket(),T_Identifier("x"),T_Assign(),T_Integer("1"),T_Repeat(),T_LeftCurlyBracket(),T_LeftBracket(),T_Integer("1"),T_Plus(),T_Integer("1"),T_RightBracket(),T_RightCurlyBracket(),T_Until(),T_Identifier("x"),T_EqualComp(),T_Integer("1"),T_RightCurlyBracket())) should equal (ProgramAST
           (List(DeclarationAST("main",0,BlockAST(List(AssignAST(0, IntLiteralAST("1")),RepeatUntilAST(BinExpAST(IntLiteralAST("1"), PlusAST(), IntLiteralAST("1")),VariableAST(0),EqualCompAST(),IntLiteralAST("1"))))))))



  }




}
