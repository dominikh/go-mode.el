package _switch

func main() {
	switch "" {
	case "foo":
		code()
	case "bar":
	case "baz":
	case "meow": // some documentation
	default:
		code()
	}
}
