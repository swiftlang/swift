// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// The index should probably use the presumed location, but for now just check
// that the indexed location isn't a mixture of both.

#sourceLocation(file: "some_file.swift", line: 1)
func testFunc() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | testFunc() | s:14swift_ide_test0C4FuncyyF | Def | rel: 0
#sourceLocation()
