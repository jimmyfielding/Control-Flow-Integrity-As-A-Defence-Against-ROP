/**
	* Generate translated program from pattern match on instructions
	* Created by Jim on 16/10/2017.
	*/

object Codegen {

	var lastVar = 0
	var instructionList = Array[ThreeAddressInstruction]()
	var currentInstruction = 0
	var binExpCount = 0
	var tabNo = "\t\t"

	/**
		*
		* @param instructions
		* @return
		*/
	def apply(instructions: Array[ThreeAddressInstruction]): String = {

		instructionList = instructions
		genCode(instructionList)

	}

	/**
		*
		* @param instructions
		* @return
		*/
	def genCode(instructions: Array[ThreeAddressInstruction]): String = {

		val builder = StringBuilder.newBuilder

		builder.append(".MODEL FLAT\n" +
			".CODE\n" +
			".intel_syntax noprefix\n" +
			"PUBLIC " + "main_entry\n")

		for (i <- instructions) {
			currentInstruction = currentInstruction + 1
			builder.append(tacMatch(i))
		}

		val result = builder.toString()

		result

	}

	/**
		*
		* @param tac
		* @return
		*/
	def tacMatch(tac: ThreeAddressInstruction): String = {
		val builder = StringBuilder.newBuilder
		tac match {
			case FunctionLabel(name, noOfArgs) =>
				builder.append(name + "_entry:" + "\tpush ebp\n" +
					"\t\tmov ebp, esp\n" +
					"\t\tsub esp, " + s"${(noOfArgs + 1) * 4}" + "\n" +
					"\t\tpush ebx\n" +
					"\t\tpush ecx\n" +
					"\t\tpush edx\n" +
					"\t\tpush esi\n" +
					"\t\tpush edi\n")
			case FunctionExit(noOfArgs) =>
				builder.append("\t\tmov esp, ebp\n" +
					"\t\tpop edi\n" +
					"\t\tpop esi\n" +
					"\t\tpop edx\n" +
					"\t\tpop ecx\n" +
					"\t\tpop ebx\n" +
					"\t\tpop ebp\n" +
					"\t\tret " + s"${(noOfArgs+1) * 4}" + "\n")
			case tac: BinaryArithmeticTAC =>
				builder.append(binArithMatch(tac.getOp, argMatch(tac.getArg1, lastVar), argMatch(tac.getArg2, lastVar)))
				if (checkIfBinExp) {
					binExpCount = binExpCount + 1
					builder.append("\t\tpush eax\n")
				} else {
					while (binExpCount != 0) {
						builder.append("\t\tpop\n")
						binExpCount = binExpCount - 1
					}
				}
			case tac: CopyTAC =>
				builder.append("\t\tmov " + argMatch(tac.getArg1, 0) + ", " + argMatch(tac.getArg2, 0) + "\n")
				lastVar = deBracket(tac.getArg1).toInt
			case tac: GoToTAC =>
				builder.append("\t\tjmp " + tac.getLabel + "\n")
			case tac: IfGoToTAC =>
				builder.append(compMatch(tac.getCond, tac.getArg1, tac.getArg2) + tac.getTLabel + "\n")
			case PutParamTAC(param) =>
				builder.append("\t\tpush " + param + "\n")
			case CallTAC(name, noOfParam) =>
				builder.append("\t\tcall " + name + "_entry\n")
			case tac: Label =>
				builder.append(tac.getName + ":\n")
			case _ =>
		}

		val result = builder.toString()

		result
	}

	/**
		*
		* @param binOp
		* @param l
		* @param r
		* @return
		*/
	def binArithMatch(binOp: Op, l: String, r: String): String = {
		val builder = StringBuilder.newBuilder
		binOp match {
			case Plus() =>
				builder.append("\t\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\t\tmov eax, " + r + "\n" +
					"\t\tadd eax, esi\n")
			case Minus() =>
				builder.append("\t\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\t\tmov eax, " + r + "\n" +
					"\t\tsub eax, esi\n")
			case Times() =>
				builder.append("\t\tmov esi, " + l + "\n" + //Consider loading all variables onto stack for the addition and not directly using registers but registers are best
					"\t\tmov eax, " + r + "\n" +
					"\t\timul eax, esi\n")
			case Div() =>

		}
		val result = builder.toString()

		result
	}

	/**
		*
		* @return
		*/
	def checkIfBinExp: Boolean = {
		var i = instructionList(currentInstruction)
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
				builder.append("\t\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\t\tmov edx, " + argMatch(r, 0) + "\n" +
					"\t\tcmp ecx, edx\n" +
					"\t\tjl ")
			case LessThanEq() =>
				builder.append("\t\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\t\tmov edx, " + argMatch(r, 0) + "\n" +
					"\t\tmov edx, eax\n" +
					"\t\tcmp ecx, edx\n" +
					"\t\tjle ")
			case EqualsCond() =>
				builder.append("\t\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\t\tmov edx, " + argMatch(r, 0) + "\n" +
					"\t\tmov edx, eax\n" +
					"\t\tcmp ecx, edx\n" +
					"\t\tjz ")
			case GreaterThanEq() =>
				builder.append("\t\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\t\tmov edx, " + argMatch(r, 0) + "\n" +
					"\t\tmov edx, eax\n" +
					"\t\tcmp ecx, edx\n" +
					"\t\tjge ")
			case GreaterThan() =>
				builder.append("\t\tmov ecx, " + argMatch(l, 0) + "\n" +
					"\t\tmov edx, " + argMatch(r, 0) + "\n" +
					"\t\tmov edx, eax\n" +
					"\t\tcmp ecx, edx\n" +
					"\t\tjg ")
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
