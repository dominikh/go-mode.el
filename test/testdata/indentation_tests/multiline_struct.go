func _() {
	testCase{char59 + "a." + char63 + "." + char63 + "." +
		char63 + ".com",
		false}

	foo := Bar{
		{
			Index:   int(index),
			MTU:     int(row.Mtu),
			DEFAULT: 123,
			CASE:    123,
		},
		{
			Index: int(index),
			MTU:   int(row.Mtu),
		},
	}

	ifi := Interface{
		Index:        int(index),
		MTU:          int(row.Mtu),
		Name:         name,
		HardwareAddr: HardwareAddr(row.PhysAddr[:row.PhysAddrLen]),
		Flags:        flags}
	ift = append(ift, ifi)

	Interface{
		{"230-Anonymous access granted, restrictions apply\n" +
			"Read the file README.txt,\n" +
			"230  please",
			23,
			230,
			"Anonymous access granted, restrictions apply\nRead the file README.txt,\n please"},
	}

	var _ struct {
		foo, // super
		bar, // important
		baz int //comments
	}

	Cool(Foo{
		Bar: Cool(Baz{
			Blah: 123,
		}),
	})

	Foo{{
		1,
	}, {
		2,
	}}

	var Foo = Bar{
		Baz: (&Blah{
			One: 1,
		}).Banana,
	}

	Foo{
		1}.Bar(
		1)
}
