// RUN: %target-run-simple-swift( -Xfrontend -enable-experimental-move-only ) | %FileCheck %s

// A small sanity check.

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
