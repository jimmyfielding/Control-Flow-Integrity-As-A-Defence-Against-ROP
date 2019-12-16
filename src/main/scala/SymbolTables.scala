import scala.collection.mutable.HashMap
import scala.annotation.tailrec

/**
	* Created by Jim on 16/10/2017.
	* Dragonbook page 89 inspired
	*/

class SymbolTables() {

	var tables: List[HashMap[String, Symbol]] = List(HashMap[String, Symbol]())

	/**
		*
		* @param s
		* @param sym
		*/
	def put(s: String, sym: Symbol): Unit = {
		tables.head.put(s, sym)
	}

	/**
		*
		* @param s
		* @return
		*/
	def getSymbol(s: String): Option[Symbol] = {
		@tailrec
		def loop(s: String, acc: List[HashMap[String, Symbol]]): Option[Symbol] = acc match {
			case Nil => None
			case h :: t => h.get(s) match {
				case Some(sym) => Some(sym)
				case None => loop(s, t)
			}
		}

		loop(s, tables)
	}

	/**
		*
		* @param s
		* @param v
		*/
	def updateValue(s: String, v: String): Unit = {
		@tailrec
		def loop(s: String, v: String, acc: List[HashMap[String, Symbol]]): Option[Symbol] = acc match {
			case Nil => None
			case h :: t => h.get(s) match {
				case Some(sym) => h.put(s, new Symbol(v, sym.tablePos))
				case None => loop(s, v, t)
			}
		}

		loop(s, v, tables)
	}

	/**
		*
		*/
	def createTable(): Unit = {
		tables = new HashMap[String, Symbol] :: tables
	}

	/**
		*
		*/
	def destroyTable(): Unit = {
		tables = tables.tail
	}

}
