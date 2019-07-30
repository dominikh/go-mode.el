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

	if bytes.Contains(out, []byte("-fsanitize")) &&
		(bytes.Contains(out, []byte("unrecognized")) ||
			bytes.Contains(out, []byte("unsupported"))) {
		return true, errors.New(string(out))
	}
}
