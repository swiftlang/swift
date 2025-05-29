// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)

// Also test this with a bridging header instead of the StdVector module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-vector.h %t2/std-vector-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-vector-bridging-header.h -cxx-interoperability-mode=upcoming-swift)

// FIXME: also run in C++20 mode when conformance works properly on UBI platform (rdar://109366764):
// %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=gnu++20)
//
// REQUIRES: executable_test

import StdlibUnittest
#if !BRIDGING_HEADER
import StdVector
#endif
import CxxStdlib

var StdVectorTestSuite = TestSuite("StdVector")

StdVectorTestSuite.test("VectorOfInt.init") {
    let v = Vector()
    expectEqual(v.size(), 0)
    expectTrue(v.empty())
}

StdVectorTestSuite.test("VectorOfInt.init(sequence)") {
    let v = Vector([])
    expectEqual(v.size(), 0)
    expectTrue(v.empty())

    let v2 = Vector([1, 2, 3])
    expectEqual(v2.size(), 3)
    expectFalse(v2.empty())
    expectEqual(v2[0], 1)
    expectEqual(v2[1], 2)
    expectEqual(v2[2], 3)
}

StdVectorTestSuite.test("VectorOfString.init(sequence)") {
    let v = VectorOfString([])
    expectEqual(v.size(), 0)
    expectTrue(v.empty())

    let v2 = VectorOfString(["", "ab", "abc"])
    expectEqual(v2.size(), 3)
    expectFalse(v2.empty())
    expectEqual(v2[0], "")
    expectEqual(v2[1], "ab")
    expectEqual(v2[2], "abc")

    let first = takesVectorOfString(["abc", "qwe"])
    expectEqual(first, "abc")
}

StdVectorTestSuite.test("VectorOfInt as ExpressibleByArrayLiteral") {
    let v: Vector = []
    expectEqual(v.size(), 0)
    expectTrue(v.empty())

    let v2: Vector = [1, 2, 3]
    expectEqual(v2.size(), 3)
    expectFalse(v2.empty())
    expectEqual(v2[0], 1)
    expectEqual(v2[1], 2)
    expectEqual(v2[2], 3)
}

#if !os(Windows) // FIXME: rdar://113704853
StdVectorTestSuite.test("VectorOfInt as MutableCollection") {
    var v = Vector([2, 3, 1])
    v.sort() // Swift function
    expectEqual(v[0], 1)
    expectEqual(v[1], 2)
    expectEqual(v[2], 3)

    v.reverse() // Swift function
    expectEqual(v[0], 3)
    expectEqual(v[1], 2)
    expectEqual(v[2], 1)
}

StdVectorTestSuite.test("VectorOfString as MutableCollection") {
    var v = VectorOfString([std.string("xyz"),
                            std.string("abc"),
                            std.string("ijk")])
    v.swapAt(0, 2) // Swift function
    expectEqual(v[0], std.string("ijk"))
    expectEqual(v[1], std.string("abc"))
    expectEqual(v[2], std.string("xyz"))

    v.reverse() // Swift function
    expectEqual(v[0], std.string("xyz"))
    expectEqual(v[1], std.string("abc"))
    expectEqual(v[2], std.string("ijk"))

    v.sort() // Swift function
    expectEqual(v[0], std.string("abc"))
    expectEqual(v[1], std.string("ijk"))
    expectEqual(v[2], std.string("xyz"))
}
#endif

StdVectorTestSuite.test("VectorOfInt.push_back") {
    var v = Vector()
    let _42: CInt = 42
    v.push_back(_42)
    expectEqual(v.size(), 1)
    expectFalse(v.empty())
    expectEqual(v[0], 42)
}

func fill(vector v: inout Vector) {
    v.push_back(1)
    v.push_back(2)
    v.push_back(CInt(3))
}

StdVectorTestSuite.test("VectorOfInt for loop") {
    var v = Vector()
    fill(vector: &v)

    var count: CInt = 1
    for e in v {
        expectEqual(e, count)
        count += 1
    }
    expectEqual(count, 4)
}

StdVectorTestSuite.test("VectorOfString for loop") {
    var v = VectorOfString()
    var count = 0
    for _ in v {
        count += 1
    }
    expectEqual(count, 0)

    v.push_back(std.string("abc"))
    v.push_back(std.string("ab"))
    for it in v {
        count += it.length()
    }
    expectEqual(count, 5)
}

StdVectorTestSuite.test("VectorOfInt.map") {
    var v = Vector()
    fill(vector: &v)

    let a = v.map { $0 + 5 }
    expectEqual(a, [6, 7, 8])
}

StdVectorTestSuite.test("VectorOfString.map") {
    var v = VectorOfString()
    v.push_back(std.string("abc"))
    v.push_back(std.string("a"))
    v.push_back(std.string("ab"))

    let a = v.map { $0.length() }
    expectEqual(a, [3, 1, 2])
}

StdVectorTestSuite.test("VectorOfInt subclass for loop") {
    var v = VectorSubclass()
    v.push_back(1)

    var count: CInt = 1
    for e in v {
        expectEqual(e, count)
        count += 1
    }
    expectEqual(count, 2)
}

StdVectorTestSuite.test("VectorOfString subclass for loop") {
    var v = VectorOfStringSubclass()
    v.push_back(std.string("abc"))

    var count: CInt = 0
    for e in v {
        expectEqual(std.string("abc"), e)
        count += 1
    }
    expectEqual(count, 1)
}

StdVectorTestSuite.test("VectorOfInt to span").require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let v = Vector([1, 2, 3])
  let s = v.span
  expectEqual(s.count, 3)
  expectFalse(s.isEmpty)
  expectEqual(s[0], 1)
  expectEqual(s[1], 2)
  expectEqual(s[2], 3)
}

runAllTests()
