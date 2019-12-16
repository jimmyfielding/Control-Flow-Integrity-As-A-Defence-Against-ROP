/**
 * Object that generators a CFG for a given set of instructions
 */
object CFGGenerator {

 /**
  * Apply method that will generate CFG
  */
  def apply(i: Array[ThreeAddressInstruction]) = {
    var cfg = new ControlFlowGraph(i)

    cfg.generateNodes
    cfg.generateJumpEdges

    cfg
  }

}
