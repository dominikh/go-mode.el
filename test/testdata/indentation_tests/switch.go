package _switch

func main() {
	switch "" {
	case "foo":
	label:
		code()
	case "bar":
	case "baz":
	case "meow": // some documentation
	default:
		code()
	}

	switch 123 {
	case 1, 2,
		3:
	case
		1,
		3:
	case
		// hi
		"hi",
		"there":
		code()
	case
		/* hi
		   there */
		"hi",
		"there":
	}

	switch {
	// attached
	case true:
		// body
		code()
		// could go either way
	case true:
	// could go either way
	case true:
		// could go both ways
	// could go both ways
	case true:

	/* this works too */
	case true:

	/* hi */
	/* this works too */
	case true:

	/* hi
	   this works too */
	case true:

	// could go either way
	case true:

		// could go either way
	case true:

	// also works
	default:
	}
}
