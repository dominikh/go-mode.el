package multilinecomment

/* Some comment here
	with my very own
		indentation as it pleases me */

func main() {
	if true {
		// code
	}
}

func _() {
	/*   foo
	 * bar
	 */

	/* abc
	123
	   def
	     lol
	*/

	/*
	   abc
	     - def
	*/

	/*
	   hello
	there */

	/*
	   hello
	   there */

	/*
	   foo
	*/

	/*
	  foo
	*/

	/*
	 foo
	*/

	/* foo
	asd
   asd
	   asd
	*/
}
