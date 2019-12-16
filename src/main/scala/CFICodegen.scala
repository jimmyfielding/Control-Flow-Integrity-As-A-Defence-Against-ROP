import scala.collection.mutable.ArrayBuffer

/**
	* Codegeneration with CFI inline pointers and checks
	*/
object CFICodegen {

	var lastVar = 0
	var currentInstruction = 0
	var binExpCount = 0
	var thisCFG: ControlFlowGraph = new ControlFlowGraph(Array[ThreeAddressInstruction]())

	/**
		* Apply method for object
		* @param cfg control flow graph
		* @return translated program
		*/
	def apply(cfg: ControlFlowGraph): String = {

		thisCFG = cfg
		genCode(thisCFG.getNodes)

	}

	/**
		* Generate x86 instructions from pattern
		* @param blocks blocks to gen
		* @return program as string
		*/
	def genCode(blocks: ArrayBuffer[Block]): String = {

		val builder = StringBuilder.newBuilder

		builder.append(".MODEL FLAT\n" +
			".CODE\n" +
			".intel_syntax noprefix\n" +
			"extern printf\n" +
			"PUBLIC " + "main_entry\n")

		for (b <- blocks) {
			currentInstruction = 0
			for (i <- b.getInstructions)
				builder.append(tacMatch(i, b))
		}

		builder.append("\tmain ENDP\n" + "cfierror: printf 1")

		val result = builder.toString()

		result

	}

	/**
		* Pattern match on instructions
		* @param tac Three Address Instruction
		* @return code as String
		*/
	def tacMatch(tac: ThreeAddressInstruction, currentBlock: Block): String = {
		val builder = StringBuilder.newBuilder
		tac match {
			case FunctionLabel(name, noOfArgs) =>
				builder.append(name + "_entry:" + "\tpush ebp\n" +
					"\tmov ebp, esp\n" +
					"\tsub esp, " + s"${noOfArgs * 4}" + "\n" +
					"\tpush ebx\n" +
					"\tpush ecx\n" +
					"\tpush edx\n" +
					"\tpush esi\n" +
					"\tpush edi\n")
			case FunctionExit(noOfArgs) =>
				builder.append("\tmov esp, ebp\n" +
					"\tpop edi\n" +
					"\tpop esi\n" +
					"\tpop edx\n" +
					"\tpop ecx\n" +
					"\tpop ebx\n" +
					"\tpop ebp\n" +
					"\tmov ecx, [esp]\n" +
					"\tadd esp, " + s"${(noOfArgs+2) * 4}" + "\n" +
					"\tmov ecx, [esp]\n" +
					"\tcmp [ecx+4], " + thisCFG.getNodes(currentBlock.getBlockNumber).getLabel + "\n" +
					"\tjne cfierror\n" +
					"\tjmp ecx\n")
			case tac: BinaryArithmeticTAC =>
				builder.append(binArithMatch(tac.getOp, argMatch(tac.getArg1, lastVar), argMatch(tac.getArg2, lastVar)))
				if (checkIfBinExp(currentBlock)) {
					binExpCount = binExpCount + 1
					builder.append("\tpush eax\n")
				} else {
					while (binExpCount != 0) {
						builder.append("\tpop\n")
						binExpCount = binExpCount - 1
					}
				}
			case tac: CopyTAC =>
				builder.append("\tmov " + argMatch(tac.getArg1, 0) + ", " + argMatch(tac.getArg2, 0) + "\n")
				lastVar = deBracket(tac.getArg1).toInt
			case GoToTAC(label) =>
				builder.append("\tjmp " + label + "\n")
			case IfGoToTAC(cond, arg1, arg2, tLabel) =>
				builder.append(compMatch(cond, arg1, arg2) + tLabel + "\n")
			case PutParamTAC(param) =>
				builder.append("\tpush " + param + "\n")
			case CallTAC(name, noOfParam) =>
				builder.append("\tcall " + name + "_entry\n" +
					"\tprefetchnta [" + thisCFG.getNodes(currentBlock.getBlockNumber).getLabel + "]\n" +
					"\tmov ecx, [esp]\n")
			case tac: Label =>
				builder.append(tac.getName + ":\n")
			case _ =>
		}

		val result = builder.toString()

		result
	}

	/**
		* Pattern match binary expressions
		* @param binOp binaryOperator
		* @param l left op
		* @param r right op
		* @return program as string
		*/
	def binArithMatch(binOp: Op, l: String, r: String): String = {
		val builder = StringBuilder.newBuilder
		binOp match {
			case Plus() =>
				builder.append("\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\tmov eax, " + r + "\n" +
					"\tadd eax, esi\n")
			case Minus() =>
				builder.append("\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\tmov eax, " + r + "\n" +
					"\tsub eax, esi\n")
			case Times() =>
				builder.append("\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\tmov eax, " + r + "\n" +
					"\timul eax, esi\n")
			//case Div() =>

		}
		val result = builder.toString()

		result
	}

	/**
		*
		* @return
		*/
	def checkIfBinExp(b: Block): Boolean = {
		var i = b.getInstructions(currentInstruction)
		i match {
			case i: BinaryArithmeticTAC => true
			case _ => false
		}
	}


	/**
		*
		* @param condition
		* @param l
		* @param r
		* @return
		*/
	def compMatch(condition: Cond, l: String, r: String): String = {
		val builder = StringBuilder.newBuilder
		condition match {
			case LessThan() =>
				builder.append("\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\tmov edx, " + argMatch(r, 0) + "\n" +
					"\tcmp ecx, edx\n" +
					"\tjl ")
			case LessThanEq() =>
				builder.append("\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\tmov edx, " + argMatch(r, 0) + "\n" +
					"\tmov edx, eax\n" +
					"\tcmp ecx, edx\n" +
					"\tjle ")
			case EqualsCond() =>
				builder.append("\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\tmov edx, " + argMatch(r, 0) + "\n" +
					"\tmov edx, eax\n" +
					"\tcmp ecx, edx\n" +
					"\tjz ")
			case GreaterThanEq() =>
				builder.append("\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\tmov edx, " + argMatch(r, 0) + "\n" +
					"\tmov edx, eax\n" +
					"\tcmp ecx, edx\n" +
					"\tjge ")
			case GreaterThan() =>
				builder.append("\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\tmov edx, " + argMatch(r, 0) + "\n" +
					"\tmov edx, eax\n" +
					"\tcmp ecx, edx\n" +
					"\tjg ")
		}
		val result = builder.toString()

		result
	}

	/**
		*
		* @param arg
		* @param offset
		* @return
		*/
	def argMatch(arg: String, offset: Int): String = {
		val builder = StringBuilder.newBuilder
		val regex = "[()]".r
		val varOrInt = regex.findFirstIn(arg)

		varOrInt match {
			case Some(s) => var i = (deBracket(arg).toInt+1) * 4; builder.append("[ebp -" + i + "]")
			case None => builder.append(arg)
		}

		val result = builder.toString()

		result
	}

	def deBracket(arg: String): String = {
		val builder = StringBuilder.newBuilder
		val stripBrackets = "[()]".r

		builder.append(stripBrackets.replaceAllIn(arg, ""))

		val result = builder.toString()

		result
	}
}
