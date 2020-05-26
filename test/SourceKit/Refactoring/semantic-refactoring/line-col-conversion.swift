// RUN: %sourcekitd-test -req=local-rename -pos=4:7 -name new_name %s -- %s

var Foo: Int {
	var missingNewlineAtEndOfFile