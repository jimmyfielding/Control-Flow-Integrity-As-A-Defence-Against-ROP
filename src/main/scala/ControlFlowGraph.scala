import scala.collection.mutable.ArrayBuffer

/**
	* Class represents control flow graph of blocks
	* @param i blocks
	*/
class ControlFlowGraph(i: Array[ThreeAddressInstruction]) {


	var nodes = ArrayBuffer[Block]()
	var currentNode: Block = new Block(0)
	var instructionList: Array[ThreeAddressInstruction] = i
	var functionCallStack: List[Int] = Nil

	/**
		*
		*/
	def generateNodes: Unit = {
		for (i <- instructionList) {
			instructionMatch(i)
		}
	}

	/**
		*
		*/
	def generateJumpEdges: Unit = {
		for (node <- nodes) {
			resolveJump(node)
		}
	}

	/**
		*
		* @param i
		*/
	def instructionMatch(i: ThreeAddressInstruction): Unit = {
		i match {
			case GoToTAC(label) => {
				currentNode.addInstruction(i)
				addBlock
			}
			case IfGoToTAC(cond, arg1, arg2, tlabel) => {
				currentNode.addInstruction(i)
				addBlock
			}
			case CopyTAC(arg1, arg2) => {
				currentNode.addInstruction(i)
				addBlock
			}
			case CallTAC(name, noOfParam) => {
				currentNode.addInstruction(i)
				addBlock
			}
			case FunctionLabel(name, noOfArgs) => {
				var blockNo = currentNode.getBlockNumber
				currentNode.addInstruction(i)

				functionCallStack = List(blockNo) ::: functionCallStack
			}
			case FunctionExit(noOfArgs) => {
				currentNode.addInstruction(i)
				currentNode.addSuccessor(functionCallStack.head + 1)
				if (functionCallStack.size > 1) {
					nodes((functionCallStack.tail.head)).addPredecessor(currentNode.getBlockNumber)
					functionCallStack = functionCallStack.tail
				}


				addBlock
			}
			case _ => currentNode.addInstruction(i)
		}
	}

	/**
		*
		*/
	def addBlock: Unit = {
		currentNode.addSuccessor(nodes.size + 1)
		nodes += currentNode
		currentNode = new Block(nodes.size)
		currentNode.addPredecessor(nodes.size - 1)
	}

	/**
		*
		* @param b
		*/
	def resolveJump(b: Block): Unit = {
		var i = b.getInstructions.last
		var n = 0

		n = searchLeaders(i)

		if (n != -1) {
			b.addSuccessor(n)
			nodes(n).addPredecessor(b.getBlockNumber)
		}

	}

	/**
		*
		* @param i
		* @return
		*/
	def searchLeaders(i: ThreeAddressInstruction): Int = {
		var blockNum = -1
		var labelToMatch = ""

		labelToMatch = branchLabel(i)
		for (n <- nodes) {
			var i1 = n.getInstructions.head
			i1 match {
				case i1: FunctionLabel => {
					if (i1.getName == labelToMatch)
						blockNum = n.getBlockNumber
				}
				case i1: Label => {
					if (i1.getName == labelToMatch)
						blockNum = n.getBlockNumber
				}
				case _ =>
			}
		}

		blockNum
	}

	/**
		*
		* @param i
		* @return
		*/
	def branchLabel(i: ThreeAddressInstruction): String = {
		i match {
			case i: IfGoToTAC => i.getTLabel
			case i: GoToTAC => i.getLabel
			case i: CallTAC => i.getName
			case _ => ""
		}
	}

	/**
		*
		* @param l
		* @return
		*/
	def stripLabel(l: String): String = {
		val stripL = "[l]".r
		stripL.replaceAllIn(l, "")
	}

	/**
		*
		* @return
		*/
	def getNodes: ArrayBuffer[Block] = {
		nodes
	}

}
