// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

func test(a: Int, b: Int = 0, c: Int = 1) { }

test(a: 0)
test(a: 0, b: 0)
test(a: 0, b: 0, c: 0)
// CHECK: test(a: 0)
// CHECK: test(a: 0, b: 0)
// CHECK: test(a: 0, b: 0, c: 0)
