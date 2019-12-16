import java.io._
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, Paths}


/**
	* Pipelined compiler with integrated (optional) CFI
	*/
object Compiler {

	/**
		*
		* @param args
		*/
	def main(args: Array[String]): Unit = {

		val sourceProgram = readFromFile(args(0))
		var isCFI = false

		if (args(1) == "Y") {
			isCFI = true
		}

		println(sourceProgram)

		Compiler(sourceProgram, isCFI)

	}

	/**
		*
		* @param s
		* @return
		*/
	def apply(s: String, cfi: Boolean): String = {
		var output = ""
		val ast = CodeAnalysis(s)

		ast match {
			case ParserErrorAST(msg) => output = msg
			case _ => {
				val x86Prog = CodeGeneration(ast, cfi)

				saveToFile(x86Prog)
				output = x86Prog
			}
		}

		println(output)

		output

	}

	def readFromFile(filename: String): String = {

		new String(Files.readAllBytes(Paths.get(filename)), UTF_8)

	}

	/**
		*
		* @param s
		*/
	def saveToFile(s: String): Unit = {

		val canonicalFilename = "program.txt"
		val file = new File(canonicalFilename)
		val bw = new BufferedWriter(new FileWriter(file))
		bw.write(s)
		bw.close()
	}

}
