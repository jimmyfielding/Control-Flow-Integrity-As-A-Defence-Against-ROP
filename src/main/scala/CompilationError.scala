
/**
  * Created by Jim on 16/10/2017.
  */

trait CompilationError {

	case class LexerError(msg: String) extends CompilationError
	case class ParserError(msg: String) extends CompilationError
	case class TACError(msg: String) extends CompilationError

}
