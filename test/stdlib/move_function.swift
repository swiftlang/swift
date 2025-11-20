// RUN: %target-run-stdlib-swift(-O -enable-experimental-feature MoveOnly)

// REQUIRES: executable_test
// REQUIRES: swift_feature_MoveOnly

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
        switch (consume x)[userHandle] {
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
        switch (x)[userHandle] {
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

tests.test("simpleVarTest") {
    var x = Klass()
    expectTrue(_isUnique_native(&x))

    var y = x
    let _ = consume y
    expectTrue(_isUnique_native(&x))
    y = Klass()
    expectTrue(_isUnique_native(&x))
}

tests.test("simpleInoutVarTest") {
    func inOutTest(_ x: inout Klass) {
        var y = x
        let _ = consume y
        expectTrue(_isUnique_native(&x))
        y = Klass()
        expectTrue(_isUnique_native(&x))
    }
    var outerX = Klass()
    inOutTest(&outerX)
}
