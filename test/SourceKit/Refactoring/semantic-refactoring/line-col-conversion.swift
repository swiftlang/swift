// RUN: %sourcekitd-test -req=refactoring.rename.local -pos=4:7 -name new_name %s -- %s

var Foo: Int {
	var missingNewlineAtEndOfFile
