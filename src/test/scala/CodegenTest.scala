import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
class SetSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}
*/

class CodegenTest extends FlatSpec with Matchers {

  they should "translate BinaryArithmeticTACs correctly" in {

  }

  they should "translate GoToTACs correctly" in {

  }

  they should "translate IfGoToTACs correctly" in {

  }

  they should "translate FunctionLabels correctly" in {

  }

  they should "translate Labels correctly" in {

  }

  they should "translate CopyTACs correctly" in {

  }

  they should "translate FunctionReturns correctly" in {

  }

  they should "translate CallTACs correctly" in {

  }

  they should "translate PutParamTACs correctly" in {

  }

  they should "translate programs correctly" in {

  }

/*
	test("A single IntLiteral should return a simple mov to the accumulator") {
    var i: ExpressionAST = new IntLiteralAST("1")
		assertResult("mov eax, 1\n") {
			MyCodegen.expMatch(i)
		}
	}

  test("A single Seq should produce code for left expression and then right expression") {
    var l: ExpressionAST = new IntLiteralAST("2")
    var r: ExpressionAST = new IntLiteralAST("3")
    var s: ExpressionAST = new SeqAST(l, r)

    assertResult("mov eax, 2\n" +
                 "mov eax, 3\n") {
      MyCodegen.expMatch(s)
    }
  }

  test("A single variable will be reserved a memory slow on the stack by incrementing the stack pointer") {
    var v: ExpressionAST = new VariableAST(4)
    assertResult("add esp, [esp + 4]\n") {
      MyCodegen.expMatch(v)
    }
  }

  test("A single assignment will change the value of the referenced memory location") {
    var i: ExpressionAST = new IntLiteralAST("2")
    var a: ExpressionAST = new AssignAST(1, i)
    assertResult("mov eax, 2\n" +
                 "mov 4[ebp], esp\n") {
      MyCodegen.expMatch(a)
    }
  }

  test("A < B should result in a jump if less than") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var lt: CompAST = new LessThanAST
    assertResult("mov eax, 1\n" +
                 "mov ecx, eax\n" +
                 "mov eax, 3\n" +
                 "mov edx, eax\n" +
                 "cmp ecx, edx\n" +
                 "jl ") {
      MyCodegen.compMatch(lt, l, r)
    }
  }

  test("A <= B should result in a jump if less than or eq") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var lte: CompAST = new LessThanEqAST
    assertResult("mov eax, 1\n" +
                 "mov ecx, eax\n" +
                 "mov eax, 3\n" +
                 "mov edx, eax\n" +
                 "cmp ecx, edx\n" +
                 "jle ") {
      MyCodegen.compMatch(lte, l, r)
    }
  }

  test("A = B should result in a jump if zero") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var eq: CompAST = new EqualCompAST
    assertResult("mov eax, 1\n" +
                 "mov ecx, eax\n" +
                 "mov eax, 3\n" +
                 "mov edx, eax\n" +
                 "cmp ecx, edx\n" +
                 "jz ") {
      MyCodegen.compMatch(eq, l, r)
    }
  }

  test("A >= B should result in a jump if greater than or eq") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var gte: CompAST = new GreaterThanEqAST
    assertResult("mov eax, 1\n" +
                 "mov ecx, eax\n" +
                 "mov eax, 3\n" +
                 "mov edx, eax\n" +
                 "cmp ecx, edx\n" +
                 "jge ") {
      MyCodegen.compMatch(gte, l, r)
    }
  }

  test("A > B should result in a jump if greater than or eq") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var gt: CompAST = new GreaterThanAST
    assertResult("mov eax, 1\n" +
                 "mov ecx, eax\n" +
                 "mov eax, 3\n" +
                 "mov edx, eax\n" +
                 "cmp ecx, edx\n" +
                 "jg ") {
      MyCodegen.compMatch(gt, l, r)
    }
  }

  test("1 + 3 should result in an add that equals 4") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var bo: BinopAST = new PlusAST
    var binEx: ExpressionAST = new BinExpAST(l, bo, r)
    assertResult("mov eax, 1\n" +
                 "mov esi, eax\n" +
                 "mov eax, 3\n" +
                 "add eax, esi\n") {
      MyCodegen.expMatch(binEx)
    }
  }

  test("3 - 1 should result in a subtraction that equals 2") {
    var l: ExpressionAST = new IntLiteralAST("3")
    var r: ExpressionAST = new IntLiteralAST("1")
    var bo: BinopAST = new MinusAST
    var binEx: ExpressionAST = new BinExpAST(l, bo, r)
    assertResult("mov eax, 3\n" +
                 "mov esi, eax\n" +
                 "mov eax, 1\n" +
                 "sub eax, esi\n") {
      MyCodegen.expMatch(binEx)
    }
  }

  test("1 * 3 should result in a multiplication that equals 3") {
    var l: ExpressionAST = new IntLiteralAST("1")
    var r: ExpressionAST = new IntLiteralAST("3")
    var bo: BinopAST = new TimesAST
    var binEx: ExpressionAST = new BinExpAST(l, bo, r)
    assertResult("mov eax, 1\n" +
                 "mov esi, eax\n" +
                 "mov eax, 3\n" +
                 "imul eax, esi\n") {
      MyCodegen.expMatch(binEx)
    }
  }

*/

}
