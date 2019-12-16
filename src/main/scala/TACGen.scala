import scala.collection.mutable.ArrayBuffer
/**
	* Created by Jim on 16/10/2017.
	*/

object TACGen {

	var symbolTables: SymbolTables = new SymbolTables()
	var threeAddressInstructions = ArrayBuffer[ThreeAddressInstruction]()
	var currentInstruction: Int = -1
	var currentVariable: Int = 0
	var binExpMatchCalls: List[Int] = List(0)
	var trueFalseLists: List[Int] = Nil

	/**
		*
		* @param progAST
		* @return
		*/
	def apply(progAST: ParserAST): Array[ThreeAddressInstruction] = {
		var ast = progAST

		ast match {
			case ast: ProgramAST =>
				genInstructions(ast)
			case _ =>
		}

		threeAddressInstructions.toArray

	}

	/**
		*
		* @param progAST
		*/
	def genInstructions(progAST: ProgramAST): Unit = {

		for (d <- progAST.decls) {
			astMatch(d)
		}

	}

	/**
		*
		* @param ast
		*/
	def astMatch(ast: ParserAST): Unit = {
		val builder = StringBuilder.newBuilder
		ast match {
			case DeclarationAST(id, numOfArgs, body) =>
				appendInstruction(new FunctionLabel(id, numOfArgs))
				for (e <- body.expressionList) {
					expMatch(e)
				}
				appendInstruction(new FunctionExit(numOfArgs))
			case _ =>
		}
	}

	/**
		*
		* @param ast
		*/
	def expMatch(ast: ExpressionAST): Unit = {
		ast match {
			case SeqAST(l, r) =>
				expMatch(l); expMatch(r)
			case AssignAST(x, e) =>
				e match {
					case e: BinExpAST => expMatch(e); appendInstruction(new CopyTAC(x.toString, s"(${currentInstruction - 1})"))
					case e: VariableAST => appendInstruction(new CopyTAC("(" + x.toString + ")", e.getOffset.toString))
					case e: IntLiteralAST => appendInstruction(new CopyTAC("(-" + x.toString + ")", e.getContents))
					case _ =>
				}
			case IfAST(l, comp, r, thenBody, elseBody) =>
				var thenName = ""
				var exitName = ""
				var bodyPos = 0
				var exitPos = 0

				appendInstruction(new IfGoToTAC(compMatch(comp), terminal(l), terminal(r), "_"))
				bodyPos = currentInstruction
				expMatch(elseBody)
				appendInstruction(new GoToTAC("_"))
				exitPos = currentInstruction
				thenName = "l" + currentInstruction
				appendInstruction(new Label(thenName))
				expMatch(thenBody)
				exitName = "l" + currentInstruction
				appendInstruction(new Label(exitName))

				writeLabel(bodyPos, thenName)
				writeLabel(exitPos, exitName)
			case WhileAST(l, comp, r, body) =>
				var ifName = ""
				var bodyName = ""
				var exitName = ""
				var ifPos = 0
				var bodyPos = 0
				var exitPos = 0


				ifName = "l" + currentInstruction
				appendInstruction(new Label(ifName))
				appendInstruction(new IfGoToTAC(compMatch(comp), terminal(l), terminal(r), "_"))
				bodyPos = currentInstruction
				appendInstruction(new GoToTAC("_"))
				exitPos = currentInstruction
				bodyName = "l" + currentInstruction
				appendInstruction(new Label(bodyName))
				for (e <- body.expressionList) {
					expMatch(e)
				}
				appendInstruction(new GoToTAC("_"))
				ifPos = currentInstruction
				exitName = "l" + currentInstruction
				appendInstruction(new Label(exitName))

				writeLabel(ifPos, bodyName)
				writeLabel(exitPos, exitName)
				writeLabel(bodyPos, ifName)

			case RepeatUntilAST(body, l, comp, r) =>
				var lname = "l" + currentInstruction
				appendInstruction(new Label(lname))
				expMatch(body)
				appendInstruction(new IfGoToTAC(compMatch(comp), terminal(l), terminal(r), lname))
			case BinExpAST(l, binop, r) =>
				var binE = new BinExpAST(l, binop, r)
				binExpMatch(binE)
			case VariableAST(x) =>
				currentVariable = terminal(ast).toInt
			case InvokeAST(name, args) =>
				var i = new InvokeAST(name, args)
				procedureCall(i)
			case _ =>
		}
	}

	/**
		*
		* @param pos
		* @param l
		*/
	def writeLabel(pos: Int, l: String): Unit = {
		var i = threeAddressInstructions(pos)
		i match {
			case i: IfGoToTAC => i.setTLabel(l)
			case i: GoToTAC => i.setLabel(l)
			case _ =>
		}
		threeAddressInstructions(pos) = i
	}

	/**
		*
		* @param ast
		* @return
		*/
	def terminal(ast: ExpressionAST): String = {
		ast match {
			case IntLiteralAST(n) =>
				n
			case VariableAST(x) =>
				"(" + x.toString + ")"
			case _ => ""
		}
	}

	/**
		*
		* @param ast
		*/
	def procedureCall(ast: InvokeAST): Unit = {
		var a = ast.args.reverse
		for (p <- a) {
			appendInstruction(new PutParamTAC(p.s))
		}
		appendInstruction(new CallTAC(ast.name, a.size))
	}

	/**
		*
		* @param t
		*/
	def appendInstruction(t: ThreeAddressInstruction): Unit = {
		threeAddressInstructions += t
		currentInstruction += 1
	}

	/**
		*
		* @param ast
		*/
	def binExpMatch(ast: BinExpAST): Unit = {
		var temp1: String = ""
		var temp2: String = ""
		var relativeL: Int = 0
		var relativeR: Int = 0

		ast.l match {
			case BinExpAST(l, binop, r) => relativeL = 1; var b = new BinExpAST(l, binop, r); binExpMatch(b)
			case IntLiteralAST(n) => temp1 = binCompMatch(ast.l)
			case VariableAST(x) => temp1 = binCompMatch(ast.l)
			case _ =>
		}
		ast.r match {
			case BinExpAST(l, binop, r) => relativeR = 1; var b = new BinExpAST(l, binop, r); binExpCall; binExpMatch(b)
			case IntLiteralAST(n) => temp2 = binCompMatch(ast.r)
			case VariableAST(x) => temp2 = binCompMatch(ast.r)
			case _ =>
		}

		relativeL match {
			case 0 => binExpMatchCalls.head match {
				case 0 => appendInstruction(new BinaryArithmeticTAC(binOpMatch(ast.binop), temp1, temp2))
				case _ => appendInstruction(new BinaryArithmeticTAC(binOpMatch(ast.binop), temp1, "(-1)"))
			}
			case _ => binExpMatchCalls.head match {
				case 0 => appendInstruction(new BinaryArithmeticTAC(binOpMatch(ast.binop), "(-1)", temp2))
				case _ => appendInstruction(new BinaryArithmeticTAC(binOpMatch(ast.binop), s"(${-1 - binExpMatchCalls.head})", "(-1)")); binExpMatchCalls = binExpMatchCalls.tail
			}

		}
	}

	/**
		*
		*/
	def binExpCall(): Unit = {
		var temp = binExpMatchCalls.head + 1
		binExpMatchCalls = binExpMatchCalls ::: List(temp)
	}

	/**
		*
		* @param ast
		* @return
		*/
	def binCompMatch(ast: ExpressionAST): String = {
		ast match {
			case IntLiteralAST(n) => n
			case VariableAST(x) => "-" + x.toString //incorrect
			case _ => ""
		}

	}

	/**
		*
		* @param ast
		* @return
		*/
	def binOpMatch(ast: BinopAST): Op = {
		ast match {
			case PlusAST() => new Plus()
			case MinusAST() => new Minus()
			case TimesAST() => new Times()
			case DivAST() => new Div()
		}
	}

	/**
		*
		* @param ast
		* @return
		*/
	def compMatch(ast: CompAST): Cond = {
		ast match {
			case LessThanAST() => new LessThan()
			case LessThanEqAST() => new LessThanEq()
			case EqualCompAST() => new EqualsCond()
			case GreaterThanAST() => new GreaterThan()
			case GreaterThanEqAST() => new GreaterThanEq()
		}

	}

	//creates a new list containing only i, an index into the array of
	//instructions; makelist returns a pointer to the newly created list.
	def makeList(i: Int): Int = {
		trueFalseLists = trueFalseLists ::: List(i)

		trueFalseLists.size
	}

	//inserts i as the target label for each of the instructions on the list pointed to by p.
	/*def backpatch(p: Int, i: Int): Array[ThreeAddressInstruction] = {
		val instructions = threeAddressInstructions.toArray

		for (i <- instructions) {
			i match {
				case ifGoToTAC => i.setTLabel(trueFalseLists.head); trueFalseLists = trueFalseLists.tail
				case goToTAC => i.setLabel(trueFalseLists.head); trueFalseLists = trueFalseLists.tail
			}
		}
		instructions
	}*/

}
