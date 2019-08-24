// RUN: %target-swift-frontend -O -emit-sil %s -Xllvm -debug-only=destructor-analysis 2>&1 | %FileCheck %s

// This test depends on asserts because we look at debug output.
// REQUIRES: asserts

class Foo {}

var a: [Int] = []
var b: [[Int]] = [[]]
var c: [Foo] = []
var d: [[Foo]] = [[]]

a.append(1)
b.append([1])
c.append(Foo())
d.append([Foo()])

// CHECK-DAG: mayStoreToMemoryOnDestruction is false: Array<Int>
// CHECK-DAG: mayStoreToMemoryOnDestruction is false: Array<Array<Int>>
// CHECK-DAG: mayStoreToMemoryOnDestruction is true: Array<Foo>
// CHECK-DAG: mayStoreToMemoryOnDestruction is true: Array<Array<Foo>>
// CHECK-DAG: mayStoreToMemoryOnDestruction is true: Array<Element>
