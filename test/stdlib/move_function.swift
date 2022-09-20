// RUN: %target-run-stdlib-swift(-Xllvm -sil-disable-pass=DestroyHoisting -Xfrontend -enable-experimental-move-only)
//
// NOTE ON ABOVE: I am disabling destroy hoisting on this test since we are
// going to move it out of the mandatory pipeline eventually and it causes the
// test to fail only when optimizations are enabled.

// REQUIRES: executable_test

import Swift
import StdlibUnittest

public class Klass {}

var tests = TestSuite("move_function_uniqueness")
defer {
    runAllTests()
}

public enum Enum {
    case foo
}

public class K2 {
    var array: [Enum] = Array(repeating: .foo, count: 10_000)

    subscript(index: Int) -> Enum {
        @inline(never)
        get {
            array[index]
        }
        @inline(never)
        set {
            array[index] = newValue
        }
        @inline(never)
        _modify {
            yield &array[index]
        }
    }
}

public class Class {
    var k2 = K2()
    var array: [Enum] = Array(repeating: .foo, count: 10_000)
}

extension Class {
    @inline(never)
    func readClassSwitchLetTest(_ userHandle: Int) {
        expectTrue(_isUnique(&self.k2))

        let x: K2
        do {
            x = self.k2
        }
        switch (_move x)[userHandle] {
        case .foo:
            expectTrue(_isUnique(&self.k2))
        }
    }
}

tests.test("readClassSwitchLetTest") {
    let c = Class()
    for f in 0..<10_000 {
        c.readClassSwitchLetTest(f)
    }
}

extension Class {
    @inline(never)
    func readArraySwitchLetTest(_ userHandle: Int) {
        expectTrue(self.array._buffer.isUniquelyReferenced())

        let x: [Enum]
        do {
            x = self.array
        }
        switch (_move x)[userHandle] {
        case .foo:
            expectTrue(self.array._buffer.isUniquelyReferenced())
        }
    }
}

tests.test("readArraySwitchLetTest") {
    let c = Class()
    for f in 0..<10_000 {
        c.readArraySwitchLetTest(f)
    }
}

tests.test("simpleArrayVarTest") {
    var x: [Enum] = Array(repeating: .foo, count: 10_000)
    expectTrue(x._buffer.isUniquelyReferenced())

    var y = x
    expectFalse(x._buffer.isUniquelyReferenced())
    let _ = _move y
    expectTrue(x._buffer.isUniquelyReferenced())
    y = []
    expectTrue(x._buffer.isUniquelyReferenced())
}

tests.test("simpleArrayInoutVarTest") {
    func inOutTest(_ x: inout [Enum]) {
        var y = x
        expectFalse(x._buffer.isUniquelyReferenced())
        let _ = _move y
        expectTrue(x._buffer.isUniquelyReferenced())
        y = []
        expectTrue(x._buffer.isUniquelyReferenced())
    }
    var outerX: [Enum] = Array(repeating: .foo, count: 10_000)
    inOutTest(&outerX)
}
