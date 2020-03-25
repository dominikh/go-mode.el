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

	1 ||
		1 || print(1,
		2,
	)

	1 ||
		1 && print(1,
			2,
		)

	f :=
		print(1,
			2,
		)

	1 +
		foo(
			1) +
		foo

	1 +
		(1 +
			1) + (1 +
		1)

	1 +
		1 + foo(
		1,
	)

	foo(
		1 && foo(
			1,
		),
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

	{
		a, b := 1,
			2
	}

	1 + foo(
		3,
	)

	foo &&
		foo && (foo &&
		foo)

	foo(1 +
		3 +
		4,
	)

	1 +
		1

	1 +
		(1 +
			1)

	1 + (1 +
		1)

	1 + (1 +
		1) +
		1

	1 + (1 +
		1) + (1 +
		1)

	1 +
		((1 +
			1) + 1) + (1 +
		1)

	1 +
		((1 +
			1) +
			1) + (1 +
		1)

	1 +
		((1 +
			1) +
			1) + (1 +
		1)

	1 +
		(1 + (1 +
			1) + (1 +
			1))

	1 +
		((1 +
			1) +
			1)

	1 + (1 + 1) +
		(1 +
			1)

	1 +
		1 + (1 +
		1)

	1 +
		1 +
		1 + (1 +
		1)

	1 +
		(1 + foo(1+
			1))

	(1 &&
		(2 &&
			(3 &&
				4))) &&
		5

	Foo{1 +
		2,
		3,
	}

	1 + (1 +
		(1 + (1 +
			1)))

	1 + (1 + (1 + (1 +
		1) +
		1) +
		1)

	1 +
		1 + Foo{1 +
		1}

	1 +
		FOo{
			1,
		}

	// foo ends the dangle, -indent
	1 +
		1 + foo(
		1,
	)

	1 +
		foo(
			1,
		)

	1 +
		(1 +
			1)

	1 +
		1 + (1 +
		1)

	1 +
		1 + 1 +
		1

	1 +
		(2 +
			(3 + 4)) + foo(
		1,
	)

	1 +
		(1 + foo(
			1,
		))

	1 +
		(2 +
			(3 + 4)) +
		1

	1 +
		(2 +
			3) + foo(
		1,
		2) +
		foo

	foo &&
		(foo && (bar && baz) &&
			qux) &&
		hi

	foo(1,
		bar(
			1,
			foo(2,
				1))) +
		foo

	foo &&
		f(bar && (foo &&
			baz)) &&
		qux

	foo(1+`,
lol`+
		123,
		456)

	"hi" + `,
lol` +
		"there"

	foo /* hi */
	bar

	1 + // hi
		2 +
		3

	1 + /* hi */
		2 +
		3

	foo ||
		foo &&
			foo(
				123,
			)

	foo ||
		foo &&
			foo{
				{
					foo: bar,
				},
			}

	foo.
		bar.
		baz.
		qux

	return 123,
		456
}
