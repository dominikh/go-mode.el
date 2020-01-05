package indentation_tests

func _() {
	foo(bar(
		baz(func() {
			qux.hi = "there"
		}),
		baz(func() {
			qux.hi = "there"
		}),
	))

	switch {
	case foo:
	}

	unrelated(t)
	foo([]int{
		123,
	}, func() {
		return
	})


	foo(
		func() {
			func() {
			}
		})

	foo(
		foo(
			1,
		))


	foo(
		1,
	)

	foo(
		foo(
			1,
		))

	foo(
		foo(
			1,
		),
	)

	foo(foo(
		1,
	))

	foo(1 +
		2)


	foo(foo(
		1,
	),
	)

	foo.
		bar(func(i int) (a b) {

		})

	foo ||
		bar &&
			baz(func() {
				X
			})

	foo &&
		func() bool {
			return X
		}()
}
