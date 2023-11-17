// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=4:7 %s -- %s | %FileCheck %s

var Foo: Int {
	var missingNewlineAtEndOfFile

// CHECK: source.edit.kind.active:
// CHECK:   4:6-4:31 source.refactoring.range.kind.basename
