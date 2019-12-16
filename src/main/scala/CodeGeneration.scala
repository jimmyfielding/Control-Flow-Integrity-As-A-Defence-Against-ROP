
/**
	* Codegeneration phase of compilation process
	*/
object CodeGeneration {

	/**
		*
		* @param p
		* @return
		*/
	def apply(p: ParserAST, cfi: Boolean): String = {

		p match {
			case ParserErrorAST(msg) => "Compilation error encountered"
			case _ =>
				val ir = TACGen(p)
				if (cfi) {
					val cfg = CFGGenerator(ir)
					val x86CFIProg = CFICodegen(cfg)
					x86CFIProg
				} else {
					val x86Prog = Codegen(ir)
					x86Prog
				}
		}
	}

}
