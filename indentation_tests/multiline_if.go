if realLength == -1 &&
	!chunked(t.TransferEncoding) &&
	bodyAllowedForStatus(t.StatusCode) {
	t.Close = true
}
