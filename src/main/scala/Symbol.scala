/**
	* Created by Jim on 16/10/2017.
	*/

class Symbol(v: String, t: Int) {
	var ofType: String = "int"
	var value: String = v
	var tablePos: Int = t

	/**
		*
		* @return
		*/
	def getValue: String = {
		v
	}

	/**
		*
		* @return
		*/
	def getTablePos: Int = {
		tablePos
	}
}
