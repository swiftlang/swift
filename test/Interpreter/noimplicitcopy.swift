// RUN: %target-run-simple-swift( -Xfrontend -enable-experimental-move-only ) | %FileCheck %s

// A small soundness check.

// REQUIRES: executable_test

import StdlibUnittest

var Tests = TestSuite("TrivialMoveOnly")
defer { runAllTests() }

func print2(_ x: Int) {
    print("printInt: \(x + 1)")
}

// CHECK: printInt: 11
// CHECK: printInt: 5
Tests.test("printInt") {
    @_noImplicitCopy let x: Int = 5
    // Infix operators where we pass values as guaranteed parameters.
    print2(x + x)
    print("printInt: \(x)")
}

// CHECK: printInt: 11
// CHECK: printInt: 6
// CHECK: printIntArg: 5
func printIntArg(@_noImplicitCopy _ x: Int) {
    print2(x + x)
    print2(x)
    print("printIntArg: \(x)")
}
Tests.test("printIntArg") {
    printIntArg(5)
}

// CHECK: printInt: 11
// CHECK: printInt: 6
// CHECK: printIntOwnedArg: 5
func printIntOwnedArg(@_noImplicitCopy _ x: __owned Int) {
    print2(x + x)
    print2(x)
    print("printIntOwnedArg: \(x)")
}
Tests.test("printIntOwnedArg") {
    printIntOwnedArg(5)
}

class Klass {
    var i = 8
    func increment() { i += 1 }
}

// CHECK: printKlass: main.Klass
// CHECK: printKlass: 10
Tests.test("printKlass") {
    @_noImplicitCopy let x = Klass()
    x.increment()
    x.increment()
    print("printKlass: \(x)")
    print("printKlass: \(x.i)")
}
