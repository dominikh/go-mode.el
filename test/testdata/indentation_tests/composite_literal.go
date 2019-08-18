package indentation_tests

func _() {
	w := struct {
		foo int
	}{
		nil,
		foo(func() {
			foo++
		}),
	}

	map[string]func(f Foo, b *Bar){
		"foo": func(f Foo, b *Bar) {
			println("hi")
		},
		"bar": func(f Foo, b *Bar) {
			println("there")
		},
	}

	Foo{
		Bar: func() {
			switch {
			case baz:
			}
		},

		"bar": {
			"foo",
			"bar",
		},
	}
}
