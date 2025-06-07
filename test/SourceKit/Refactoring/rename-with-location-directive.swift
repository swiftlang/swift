#sourceLocation(file: "someFile.swift", line: 10)
func test() {}
#sourceLocation()

// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=2:6 %s -- %s | %FileCheck %s
// CHECK: 2:6-2:10 source.refactoring.range.kind.basename
