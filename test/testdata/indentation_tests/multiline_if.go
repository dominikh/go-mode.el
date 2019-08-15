package foo

import (
	"bytes"
	"errors"
)

func _() {
	if realLength == -1 &&
		!chunked(t.TransferEncoding) &&
		bodyAllowedForStatus(t.StatusCode) {
		t.Close = true
	}

	if true &&
		(true ||
			true && (true ||
				false) && true) {
		true
	}

	if true && (true &&
		true) {
		true
	}

	if true &&
		(true && (true ||
			false) && true) {
		true
	}

	if true &&
		foo(true &&
			true) {
		true
	}

	if true &&
		true && (true ||
		true) {
		true
	}

	if (true &&
		true) &&
		true {
		true
	}

	if bytes.Contains(out, []byte("-fsanitize")) &&
		(bytes.Contains(out, []byte("unrecognized")) ||
			bytes.Contains(out, []byte("unsupported"))) {
		return true, errors.New(string(out))
	}

	if true ==
		false {
		return
	}

	if true !=
		false {
		return
	}

	if foo(1, // hi
		// hi

		2) { // hi
		return
	}

	if foo(
		func() {
		}) {
		return
	}

	if foo == 0 ||
		!foo.Bar(
			"some",
			"args") {
		return
	}

	if true {
		break
	} else if true {
		if true {
			break
		}
	}

	if true {
	} else if true ||
		true {
		return
	}

	if 1 +
		1 {
		X
	}

	if 1 +
		(1 +
			1) {
		X
	}

	if 1 +
		(1 +
			1) +
		1 {
		X
	}

	if 1 +
		1 +
		1 + (1 +
		1) {
		X
	}

	if 1 +
		(1 +
			1) + (1 +
		1) {
		X
	}

	if (Foo{1,
		1}).Bar {
		return
	}

	if foo ||
		foo &&
			foo ==
				foo+
					foo*
						foo {
		foo
	}

	if foo() ||
		foo() &&
			foo() {
		foo
	}
}
