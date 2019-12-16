
/**
	* Code analysis phase of compilation
	*/
object CodeAnalysis {

	/**
		*
		* @param s
		* @return
		*/
	def apply(s: String): ParserAST = {

		val tokens = Lexer(s)
		println(tokens)
		val ast = Parser(tokens)

		ast
	}

}
