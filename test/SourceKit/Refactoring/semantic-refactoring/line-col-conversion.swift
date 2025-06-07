// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=5:7 %s -- %s | %FileCheck %s

var Foo: Int {
	var missingNewlineAtEndOfFile

// CHECK: source.edit.kind.active:
// CHECK:   5:6-5:31 source.refactoring.range.kind.basename
