package main

func init() {
	a :=
		123

	print(123,
		456,
		789,
	)

	c :=
		"foo"

	d := // meh
		123

	e := /* meh
		another meh
		*/
		123

	println(123, /* foo
		bar */
		456,
	)

	f :=
		print(1,
			2,
		)

	g :=
		int64(4 *
			3 *
			1)

	i :=
		"" != "" ||
			true == false ||
			false == false

	a, b :=
		1,
		2

	a,
		b := 1, 2

	return 123,
		456
}
