// RUN: %target-run-stdlib-swift(-Xllvm -sil-disable-pass=DestroyHoisting)
//
// NOTE ON ABOVE: I am disabling destroy hoisting on this test since we are
// going to move it out of the mandatory pipeline eventually and it causes the
// test to fail only when optimizations are enabled.

// REQUIRES: executable_test

import Swift
import StdlibUnittest

public class Klass {}

var tests = TestSuite("move_function_uniqueness")

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
    func readClassTest(_ userHandle: Int) {
        assert(_isUnique(&self.k2))

        let x: K2
        do {
            x = self.k2
        }
        switch _move(x)[userHandle] {
        case .foo:
            assert(_isUnique(&self.k2))
        }
    }
}

tests.test("classUniquenessTest") {
    let c = Class()
    for f in 0..<10_000 {
        c.readClassTest(f)
    }
}

extension Class {
    @inline(never)
    func readArrayTest(_ userHandle: Int) {
        assert(self.array._buffer.isUniquelyReferenced())

        let x: [Enum]
        do {
            x = self.array
        }
        switch _move(x)[userHandle] {
        case .foo:
            assert(self.array._buffer.isUniquelyReferenced())
        }
    }
}

tests.test("arrayUniquenessTest") {
    let c = Class()
    for f in 0..<10_000 {
        c.readArrayTest(f)
    }
}

runAllTests()
