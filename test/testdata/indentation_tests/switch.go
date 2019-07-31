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
}
