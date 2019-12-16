import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

/**
	* Created by Jim on 16/10/2017.
	*/

object Parser extends Parsers with CompilationError {

	override type Elem = LexerToken

	/**
		*
		* @param tokens
		*/
	class LexerTokenReader(tokens: Seq[LexerToken]) extends Reader[LexerToken] {

		override def first: LexerToken = tokens.head

		override def atEnd: Boolean = tokens.isEmpty

		override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

		override def rest: Reader[LexerToken] = new LexerTokenReader(tokens.tail)

	}

	/**
		*
		* @param tokens
		* @return
		*/
	def apply(tokens: Seq[LexerToken]): ParserAST = {
		val reader = new LexerTokenReader(tokens)
		program(reader) match {
			case NoSuccess(msg, next) => new ParserErrorAST(msg)
			case Success(result, next) => result
		}
	}

	var symTables: SymbolTables = new SymbolTables()

	/**
		*
		* @return
		*/
	def program: Parser[ProgramAST] = {

		rep1(declaration) ^^ { case decs => new ProgramAST(decs) }

	}

	/**
		*
		* @return
		*/
	def declaration: Parser[DeclarationAST] = {

		symTables.createTable
		(T_Def() ~ identifier ~ T_LeftBracket() ~ (rep(identifier <~ T_Comma()) ~ identifier).? ~ T_RightBracket() ~ T_Equal() ~ block) ^^ {
			case a ~ b ~ c ~ d ~ e ~ f ~ g => new DeclarationAST(b.s, d.size, g)
		}
	}

	/**
		*
		* @return
		*/
	def block: Parser[BlockAST] = {
		(T_LeftCurlyBracket() ~ rep1(expression) ~ T_RightCurlyBracket()) ^^ {
			case a ~ b ~ c => new BlockAST(b)
		}
	}

	/**
		*
		* @return
		*/
	def `if`: Parser[IfAST] = {
		(T_If() ~ expression ~ comp ~ expression ~ T_Then() ~ T_LeftCurlyBracket() ~ expression ~ T_RightCurlyBracket() ~ T_Else() ~ T_LeftCurlyBracket() ~ expression ~ T_RightCurlyBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h ~ i ~ j ~ k ~ l => new IfAST(b, c, d, g, k)
		}
	}

	/**
		*
		* @return
		*/
	def `while`: Parser[WhileAST] = {
		(T_While() ~ expression ~ comp ~ expression ~ T_Do() ~ block) ^^ {
			case a ~ b ~ c ~ d ~ e ~ f => new WhileAST(b, c, d, f)
		}
	}

	/**
		*
		* @return
		*/
	def repeatUntil: Parser[RepeatUntilAST] = {
		(T_Repeat() ~ T_LeftCurlyBracket() ~ expression ~ T_RightCurlyBracket() ~ T_Until() ~ expression ~ comp ~ expression) ^^ {
			case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h => new RepeatUntilAST(c, f, g, h)
		}
	}

	/**
		*
		* @return
		*/
	def binExp: Parser[BinExpAST] = {
		val intLIntL = (T_LeftBracket() ~ intLiteral ~ binop ~ intLiteral ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val intLVar = (T_LeftBracket() ~ intLiteral ~ binop ~ variable ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val intLBinExp = (T_LeftBracket() ~ intLiteral ~ binop ~ binExp ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val varIntL = (T_LeftBracket() ~ variable ~ binop ~ intLiteral ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val varVar = (T_LeftBracket() ~ variable ~ binop ~ variable ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val varBinExp = (T_LeftBracket() ~ variable ~ binop ~ binExp ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val binExpIntL = (T_LeftBracket() ~ binExp ~ binop ~ intLiteral ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val binExpVar = (T_LeftBracket() ~ binExp ~ binop ~ variable ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		val binExpBinExp = (T_LeftBracket() ~ binExp ~ binop ~ binExp ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new BinExpAST(b, c, d)
		}

		intLIntL | intLVar | intLBinExp | varIntL | varVar | varBinExp | binExpIntL | binExpVar | binExpBinExp
	}

	/**
		*
		* @return
		*/
	def assign: Parser[AssignAST] = {
		(identifier ~ T_Assign() ~ expression) ^^ {
			case a ~ b ~ c => {
				symTables.getSymbol(a.s) match {
					case Some(sym) => symTables.updateValue(a.s, sym.getValue); new AssignAST(sym.tablePos, c)
					case None => symTables.tables.head.put(a.s, new Symbol(extractValue(c), 0)); new AssignAST(0, c)
				}
			}
		}
	}

	/**
		*
		* @param ast
		* @return
		*/
	def extractValue(ast: ExpressionAST): String = {
		ast match {
			case IntLiteralAST(s) => s
			case VariableAST(x) => x.toString
			case _ => ""
		}
	}

	/**
		*
		* @return
		*/
	def variable: Parser[VariableAST] = {
		identifier ^^ {
			case a => {
				symTables.getSymbol(a.s) match {
					case Some(sym) => new VariableAST(sym.tablePos)
					case None => new ParserError("Syntax error encountered."); new VariableAST(-1)
				}
			}
		}
	}

	/**
		*
		* @return
		*/
	def seq: Parser[SeqAST] = {
		(T_LeftBracket() ~ expression ~ T_SemiColon() ~ expression ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => new SeqAST(b, d)
		}
	}

	/**
		*
		* @return
		*/
	def invoke: Parser[InvokeAST] = {
		(identifier ~ T_LeftBracket() ~ (parameter <~ T_Comma()).* ~ parameter.* ~ T_RightBracket()) ^^ {
			case a ~ b ~ c ~ d ~ e => {
				var l = c ::: d
				if(!(symTables.tables.isEmpty)) {
					symTables.destroyTable
				}
				symTables.createTable
				for(p <- l){
					symTables.getSymbol(p.s)
				}
				new InvokeAST(a.s, l)
			}
		}
	}

	def parameter: Parser[ParameterAST] = {
		(identifier|intLiteral) ^^ {
			case a => a match {
				case a: IdentifierAST => {
					symTables.getSymbol(a.s) match {
						case Some(sym) => new ParameterAST(sym.getValue)
						case None => new ParameterAST("-1")
					}
				}
				case a: IntLiteralAST => {
					new ParameterAST(a.s)
				}
			}
		}
	}

	/**
		*
		* @return
		*/
	def expression: Parser[ExpressionAST] = {

		`if` | `while` | repeatUntil | binExp | assign | invoke | variable | seq | intLiteral

	}

	/**
		*
		* @return
		*/
	def binop: Parser[BinopAST] = {
		val plus = T_Plus() ^^ (_ => PlusAST())
		val times = T_Times() ^^ (_ => TimesAST())
		val minus = T_Minus() ^^ (_ => MinusAST())
		val div = T_Div() ^^ (_ => DivAST())

		plus | times | minus | div
	}

	/**
		*
		* @return
		*/
	def comp: Parser[CompAST] = {
		val lessThan = T_LessThan() ^^ (_ => LessThanAST())
		val lessThanEq = T_LessThanEq() ^^ (_ => LessThanEqAST())
		val equalComp = T_EqualComp() ^^ (_ => EqualCompAST())
		val greaterThanEq = T_GreaterThanEq() ^^ (_ => GreaterThanEqAST())
		val greaterThan = T_GreaterThan() ^^ (_ => GreaterThanAST())

		lessThan | lessThanEq | equalComp | greaterThanEq | greaterThan

	}

	/**
		*
		* @return
		*/
	def identifier: Parser[IdentifierAST] = {
		accept("identifier", { case id@T_Identifier(s) => IdentifierAST(s) })
	}

	/**
		*
		* @return
		*/
	def intLiteral: Parser[IntLiteralAST] = {
		accept("intLiteral", { case intLit@T_Integer(s) => IntLiteralAST(s) })
	}

}
