
/**
	* Class representing a basic block
	* @param i	block number
	*/
class Block(i: Int) {

	var blockNumber = i
	var instructions: List[ThreeAddressInstruction] = Nil
	var successors: List[Int] = Nil
	var predecessors: List[Int] = Nil
	var uniqueLabel = 0x00000000 + blockNumber

	/**
		* Add's edge in graph for block to branch to
		* @param s block number of successor
		*/
	def addSuccessor(s: Int): Unit = {
		successors = successors ::: List(s)
	}

	/**
		* Add's edge in graph for block to be branched from
		* @param p block number of predecessor
		*/
	def addPredecessor(p: Int): Unit = {
		predecessors = predecessors ::: List(p)
	}

	/**
		* Add instruction to block
		* @param i instruction to be added
		*/
	def addInstruction(i: ThreeAddressInstruction): Unit = {
		instructions = instructions ::: List(i)
	}

	/**
		* Getter for instructions
		* @return instructions
		*/
	def getInstructions: List[ThreeAddressInstruction] = {
		instructions
	}

	/**
		* Getter for block number
		* @return block number
		*/
	def getBlockNumber: Int = {
		blockNumber
	}

	/**
	 * Getter for getLabel
	 * @return getLabel
	 */
	def getLabel: String = {
		uniqueLabel.toString
	}

}
