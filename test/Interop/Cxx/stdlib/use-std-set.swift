// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// Also test this with a bridging header instead of the StdSet module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-set.h %t2/std-set-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-set-bridging-header.h -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-set-bridging-header.h -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-set-bridging-header.h -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing other C++ stdlibs: rdar://87654514
// REQUIRES: OS=macosx || OS=linux-gnu
// UNSUPPORTED: LinuxDistribution=fedora-41

import StdlibUnittest
#if !BRIDGING_HEADER
import StdSet
#endif
import CxxStdlib
import Cxx

var StdSetTestSuite = TestSuite("StdSet")

StdSetTestSuite.test("iterate over Swift.Array") {
    let s = Array(initSetOfCInt())
    var result = [CInt]()
    for x in s {
        result.append(x)
    }
    expectEqual(result[0], 1)
    expectEqual(result[1], 3)
    expectEqual(result[2], 5)
}

StdSetTestSuite.test("SetOfCInt.contains") {
    // This relies on the `std::set` conformance to `CxxSet` protocol.
    let s = initSetOfCInt()
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))
}

StdSetTestSuite.test("UnorderedSetOfCInt.contains") {
    // This relies on the `std::unordered_set` conformance to `CxxSet` protocol.
    let s = initUnorderedSetOfCInt()
    expectFalse(s.contains(1))
    expectTrue(s.contains(2))
    expectFalse(s.contains(3))
}

StdSetTestSuite.test("MultisetOfCInt.contains") {
    // This relies on the `std::multiset` conformance to `CxxSet` protocol.
    let s = initMultisetOfCInt()
    expectFalse(s.contains(1))
    expectTrue(s.contains(2))
    expectFalse(s.contains(3))
}

StdSetTestSuite.test("SetOfCInt.init()") {
    let s = SetOfCInt([1, 3, 5])
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))
}

StdSetTestSuite.test("UnorderedSetOfCInt.init()") {
    let s = UnorderedSetOfCInt([1, 3, 5])
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))
}

StdSetTestSuite.test("SetOfCInt as ExpressibleByArrayLiteral") {
    let s: SetOfCInt = [1, 3, 5]
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))

    func takesSetOfCInt(_ s: SetOfCInt) {
        expectTrue(s.contains(1))
        expectTrue(s.contains(2))
        expectFalse(s.contains(3))
    }

    takesSetOfCInt([1, 2])
}

StdSetTestSuite.test("UnorderedSetOfCInt as ExpressibleByArrayLiteral") {
    let s: UnorderedSetOfCInt = [1, 3, 5]
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))

    func takesUnorderedSetOfCInt(_ s: UnorderedSetOfCInt) {
        expectTrue(s.contains(1))
        expectTrue(s.contains(2))
        expectFalse(s.contains(3))
    }

    takesUnorderedSetOfCInt([1, 2])
}

StdSetTestSuite.test("MultisetOfCInt as ExpressibleByArrayLiteral") {
    let s: MultisetOfCInt = [1, 1, 3]
    expectTrue(s.contains(1))
    expectFalse(s.contains(2))
    expectTrue(s.contains(3))

    func takesMultisetOfCInt(_ s: MultisetOfCInt) {
        expectTrue(s.contains(1))
        expectTrue(s.contains(2))
        expectFalse(s.contains(3))
    }

    takesMultisetOfCInt([1, 1, 2])
}

StdSetTestSuite.test("SetOfCInt.insert") {
    var s = SetOfCInt()
    expectFalse(s.contains(123))

    let res1 = s.insert(123)
    expectTrue(res1.inserted)
    expectTrue(s.contains(123))

    let res2 = s.insert(123)
    expectFalse(res2.inserted)
    expectTrue(s.contains(123))
}

StdSetTestSuite.test("UnorderedSetOfCInt.insert") {
    var s = UnorderedSetOfCInt()
    expectFalse(s.contains(123))

    let res1 = s.insert(123)
    expectTrue(res1.inserted)
    expectTrue(s.contains(123))

    let res2 = s.insert(123)
    expectFalse(res2.inserted)
    expectTrue(s.contains(123))
}

StdSetTestSuite.test("SetOfCInt.erase") {
    var s = initSetOfCInt()
    expectTrue(s.contains(1))
    s.erase(1)
    expectFalse(s.contains(1))
    s.erase(1)
    expectFalse(s.contains(1))
}

StdSetTestSuite.test("UnorderedSetOfCInt.erase") {
    var s = initUnorderedSetOfCInt()
    expectTrue(s.contains(2))
    s.erase(2)
    expectFalse(s.contains(2))
    s.erase(2)
    expectFalse(s.contains(2))
}

StdSetTestSuite.test("SetOfCInt.remove") {
    var s = initSetOfCInt()
    expectTrue(s.contains(1))
    expectEqual(s.remove(1), 1)
    expectFalse(s.contains(1))
    expectEqual(s.remove(1), nil)
    expectFalse(s.contains(1))
}

StdSetTestSuite.test("UnorderedSetOfCInt.remove") {
    var s = initUnorderedSetOfCInt()
    expectTrue(s.contains(2))
    expectEqual(s.remove(2), 2)
    expectFalse(s.contains(2))
    expectEqual(s.remove(2), nil)
    expectFalse(s.contains(2))
}

StdSetTestSuite.test("SetOfCInt.filter") {
    let s1 = initSetOfCInt()
      .filter { $0 % 2 != 0 }

    expectTrue(s1.contains(1))
    expectTrue(s1.contains(3))
    expectTrue(s1.contains(5))

    let s2 = initSetOfCInt()
        .filter { $0 > 3 }

    expectFalse(s2.contains(1))
    expectFalse(s2.contains(3))
    expectTrue(s2.contains(5))
}

StdSetTestSuite.test("UnorderedSetOfCInt.filter") {
    let s1 = initUnorderedSetOfCInt()
        .filter { $0 % 2 != 0 }

    expectFalse(s1.contains(2))
    expectFalse(s1.contains(4))
    expectFalse(s1.contains(6))

    let s2 = initUnorderedSetOfCInt()
      .filter { $0 > 3 }
    expectFalse(s2.contains(2))
    expectTrue(s2.contains(4))
    expectTrue(s2.contains(6))
}

StdSetTestSuite.test("SetOfCInt.union(cxxset)") {
    let m = initSetOfCInt()
    let n = initSetOfCInt2()
    let result = m.union(n)
    expectEqual(result.count, 4)
    expectTrue(result.contains(1))
    expectTrue(result.contains(3))
    expectTrue(result.contains(5))
    expectTrue(result.contains(7))
}

StdSetTestSuite.test("SetOfCInt.union(sequence)") {
    let m = initSetOfCInt()
    let n = m.union([CInt(3), 7, 9])
    expectEqual(n.count, 5)
    expectTrue(n.contains(1))
    expectTrue(n.contains(3))
    expectTrue(n.contains(5))
    expectTrue(n.contains(7))
    expectTrue(n.contains(9))
}

StdSetTestSuite.test("SetOfCInt.union(sequence).withDuplicates") {
    let m = initSetOfCInt()
    let n = m.union([CInt(3), 3, 7, 9, 9, 9])
    expectEqual(n.count, 5)
    expectTrue(n.contains(1))
    expectTrue(n.contains(3))
    expectTrue(n.contains(5))
    expectTrue(n.contains(7))
    expectTrue(n.contains(9))
}

StdSetTestSuite.test("UnorderedSetOfCInt.union(cxxset)") {
    let m = initUnorderedSetOfCInt()
    let n = initUnorderedSetOfCInt2()
    let result = m.union(n)
    expectEqual(result.count, 4)
    expectTrue(result.contains(2))
    expectTrue(result.contains(4))
    expectTrue(result.contains(6))
    expectTrue(result.contains(8))
}

StdSetTestSuite.test("UnorderedSetOfCInt.union(sequence)") {
    let m = initUnorderedSetOfCInt()
    let result = m.union([CInt(4), 8, 10])
    expectEqual(result.count, 5)
    expectTrue(result.contains(2))
    expectTrue(result.contains(4))
    expectTrue(result.contains(6))
    expectTrue(result.contains(8))
    expectTrue(result.contains(10))
}

StdSetTestSuite.test("UnorderedSetOfCInt.union(sequence).withDuplicates") {
    let m = initUnorderedSetOfCInt()
    let result = m.union([CInt(4), 4, 8, 8, 10, 10, 10])
    expectEqual(result.count, 5)
    expectTrue(result.contains(2))
    expectTrue(result.contains(4))
    expectTrue(result.contains(6))
    expectTrue(result.contains(8))
    expectTrue(result.contains(10))
}

StdSetTestSuite.test("SetOfCInt.formUnion(sequence)") {
    var s = initSetOfCInt()

    s.formUnion([2, 4, 6])

    for i in CInt(1)...6 {
        expectTrue(s.contains(i))
    }
}

StdSetTestSuite.test("SetOfCInt.formUnion(sequence).withDuplicates") {
    var s = initSetOfCInt()

    s.formUnion([2, 2, 4, 4, 6, 6, 6])

    expectEqual(s.count, 6)
    for i in CInt(1)...6 {
        expectTrue(s.contains(i))
    }
}

StdSetTestSuite.test("UnorderedSetOfCInt.formUnion(sequence)") {
    var s = initUnorderedSetOfCInt()

    s.formUnion([1, 3, 5])

    for i in CInt(1)...6 {
        expectTrue(s.contains(i))
    }
}

StdSetTestSuite.test("UnorderedSetOfCInt.formUnion(sequence).withDuplicates") {
    var s = initUnorderedSetOfCInt()

    s.formUnion([1, 1, 3, 3, 3, 5, 5])

    expectEqual(s.count, 6)
    for i in CInt(1)...6 {
        expectTrue(s.contains(i))
    }
}

StdSetTestSuite.test("SetOfCInt.intersection(cxxset)") {
    let s = initSetOfCInt()

    let r1 = s.intersection(initSetOfCInt())

    expectTrue(r1.contains(1))
    expectTrue(r1.contains(3))
    expectTrue(r1.contains(5))

    let r2 = s.intersection(initSetOfCInt2())
    expectFalse(r2.contains(1))
    expectTrue(r2.contains(3))
    expectFalse(r2.contains(5))
}

StdSetTestSuite.test("UnorderedSetOfCInt.intersection(cxxset)") {
    let s = initUnorderedSetOfCInt()

    let r1 = s.intersection(initUnorderedSetOfCInt())
    expectTrue(r1.contains(2))
    expectTrue(r1.contains(4))
    expectTrue(r1.contains(6))

    let r2 = s.intersection(initUnorderedSetOfCInt2())
    expectFalse(r2.contains(2))
    expectTrue(r2.contains(4))
    expectFalse(r2.contains(6))
}

StdSetTestSuite.test("SetOfCInt.intersection(sequence)") {
    let s = initSetOfCInt()

    let r1 = s.intersection([2, 4, 6])
    expectTrue(r1.isEmpty)

    let r2 = s.intersection([1, 5])
    expectTrue(r2.contains(1))
    expectFalse(r2.contains(3))
    expectTrue(r2.contains(5))
}

StdSetTestSuite.test("SetOfCInt.intersection(sequence).withDuplicates") {
    let s = initSetOfCInt()

    let r1 = s.intersection([2, 2, 4, 4, 6, 6])
    expectTrue(r1.isEmpty)

    let r2 = s.intersection([1, 1, 3, 3, 3, 5, 5])
    expectTrue(r2.contains(1))
    expectTrue(r2.contains(3))
    expectTrue(r2.contains(5))
    expectEqual(r2.count, 3)
}

StdSetTestSuite.test("UnorderedSetOfCInt.intersection(sequence)") {
    let s = initUnorderedSetOfCInt()

    let r1 = s.intersection([1, 3, 5])
    expectTrue(r1.isEmpty)

    let r2 = s.intersection([2, 6])
    expectTrue(r2.contains(2))
    expectFalse(r2.contains(4))
    expectTrue(r2.contains(6))
}

StdSetTestSuite.test("UnorderedSetOfCInt.intersection(sequence).withDuplicates") {
    let s = initUnorderedSetOfCInt()

    let r1 = s.intersection([1, 1, 3, 3, 5, 5])
    expectTrue(r1.isEmpty)

    let r2 = s.intersection([2, 2, 4, 4, 4, 6, 6])
    expectTrue(r2.contains(2))
    expectTrue(r2.contains(4))
    expectTrue(r2.contains(6))
    expectEqual(r2.count, 3)
}

StdSetTestSuite.test("SetOfCInt.formIntersection(sequence)") {
    var s = initSetOfCInt()

    s.formIntersection([1, 5])
    expectTrue(s.contains(1))
    expectFalse(s.contains(3))
    expectTrue(s.contains(5))

    s.formIntersection([2, 4, 6])
    expectTrue(s.isEmpty)
}

StdSetTestSuite.test("SetOfCInt.formIntersection(sequence).withDuplicates") {
    var s = initSetOfCInt()

    s.formIntersection([1, 1, 3, 3, 3, 5, 5])
    expectTrue(s.contains(1))
    expectTrue(s.contains(3))
    expectTrue(s.contains(5))
    expectEqual(s.count, 3)
}

StdSetTestSuite.test("UnorderedSetOfCInt.formIntersection(sequence)") {
    var s = initUnorderedSetOfCInt()

    s.formIntersection([2, 6])

    expectTrue(s.contains(2))
    expectFalse(s.contains(4))
    expectTrue(s.contains(6))

    s.formIntersection([1, 3, 5])
    expectTrue(s.isEmpty)
}

StdSetTestSuite.test("UnorderedSetOfCInt.formIntersection(sequence).withDuplicates") {
    var s = initUnorderedSetOfCInt()

    s.formIntersection([2, 2, 4, 4, 4, 6, 6])
    expectTrue(s.contains(2))
    expectTrue(s.contains(4))
    expectTrue(s.contains(6))
    expectEqual(s.count, 3)
}

StdSetTestSuite.test("SetOfCInt.subtracting(cxxset)") {
    let m = initSetOfCInt()
    let n = initSetOfCIntSubset()
    let result = m.subtracting(n)
    expectEqual(result.count, 1)
    expectTrue(result.contains(3))
    expectFalse(result.contains(1))
    expectFalse(result.contains(5))
}

StdSetTestSuite.test("SetOfCInt.subtracting(sequence)") {
    let m = initSetOfCInt()
    let n = m.subtracting([CInt(3)])
    expectEqual(n.count, 2)
    expectTrue(n.contains(1))
    expectTrue(n.contains(5))
    expectFalse(n.contains(3))
}

StdSetTestSuite.test("SetOfCInt.subtracting(sequence).withDuplicates") {
    let m = initSetOfCInt()
    let n = m.subtracting([CInt(1), 1, 3, 3, 3])
    expectEqual(n.count, 1)
    expectFalse(n.contains(1))
    expectTrue(n.contains(5))
    expectFalse(n.contains(3))
}

StdSetTestSuite.test("UnorderedSetOfCInt.subtracting(cxxset)") {
    let m = initUnorderedSetOfCInt()
    let n = initUnorderedSetOfCIntSubset()
    let result = m.subtracting(n)
    expectEqual(result.count, 1)
    expectTrue(result.contains(6))
    expectFalse(result.contains(2))
    expectFalse(result.contains(4))
}

StdSetTestSuite.test("UnorderedSetOfCInt.subtracting(sequence)") {
    let m = initUnorderedSetOfCInt()
    let n = m.subtracting([CInt(4)])
    expectEqual(n.count, 2)
    expectTrue(n.contains(2))
    expectTrue(n.contains(6))
    expectFalse(n.contains(4))
}

StdSetTestSuite.test("UnorderedSetOfCInt.subtracting(sequence).withDuplicates") {
    let m = initUnorderedSetOfCInt()
    let n = m.subtracting([CInt(2), 2, 4, 4, 4])
    expectEqual(n.count, 1)
    expectTrue(n.contains(6))
    expectFalse(n.contains(2))
    expectFalse(n.contains(4))
}

StdSetTestSuite.test("SetOfCInt.subtract(cxxset)") {
    var s = initSetOfCInt()
    let other = initSetOfCIntSubset()

    s.subtract(other)

    expectFalse(s.contains(1))
    expectTrue(s.contains(3))
    expectFalse(s.contains(5))
}

StdSetTestSuite.test("SetOfCInt.subtract(sequence)") {
    var s = initSetOfCInt()

    s.subtract([1, 3])

    expectFalse(s.contains(1))
    expectFalse(s.contains(3))
    expectTrue(s.contains(5))

    s.subtract([5])
    expectFalse(s.contains(5))
}

StdSetTestSuite.test("SetOfCInt.subtract(sequence).withDuplicates") {
    var s = initSetOfCInt()

    s.subtract([1, 1, 3, 3, 3])

    expectFalse(s.contains(1))
    expectFalse(s.contains(3))
    expectTrue(s.contains(5))
}

StdSetTestSuite.test("UnorderedSetOfCInt.subtract(cxxset)") {
    var s = initUnorderedSetOfCInt()
    let other = initUnorderedSetOfCIntSubset()

    s.subtract(other)

    expectFalse(s.contains(2))
    expectFalse(s.contains(4))
    expectTrue(s.contains(6))
}

StdSetTestSuite.test("UnorderedSetOfCInt.subtract(sequence)") {
    var s = initUnorderedSetOfCInt()

    s.subtract([2, 4])

    expectFalse(s.contains(2))
    expectFalse(s.contains(4))
    expectTrue(s.contains(6))

    s.subtract([6])
    expectFalse(s.contains(6))
}

StdSetTestSuite.test("SetOfCInt.isSubset(cxxset)") {
  let m = initSetOfCInt()

  expectFalse(m.isSubset(of: initSetOfCIntEmpty()))
  expectFalse(m.isSubset(of: initSetOfCIntSubset()))
  expectTrue(m.isSubset(of: m))
  expectTrue(m.isSubset(of: initSetOfCIntSuperset()))
  expectFalse(m.isSubset(of: initSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("SetOfCInt.isSubset(sequence)") {
  let m = initSetOfCInt()

  expectFalse(m.isSubset(of: Array<CInt>()))
  expectFalse(m.isSubset(of: [CInt(1)]))
  expectFalse(m.isSubset(of: [CInt(1), 3]))
  expectTrue(m.isSubset(of: [CInt(1), 3, 5]))
  expectTrue(m.isSubset(of: [CInt(1), 3, 5, 7]))
  expectFalse(m.isSubset(of: [CInt(1), 5, 7]))
}

StdSetTestSuite.test("SetOfCInt.isSubset(sequence).withDuplicates") {
  let m = initSetOfCInt()

  expectTrue(m.isSubset(of: [CInt(1), 3, 5, 5, 5]))
  expectTrue(m.isSubset(of: [CInt(1), 1, 3, 3, 5, 7, 7]))
  expectFalse(m.isSubset(of: [CInt(1), 1, 5, 5, 7, 7]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isSubset(cxxset)") {
  let m = initUnorderedSetOfCInt()

  expectFalse(m.isSubset(of: initUnorderedSetOfCIntEmpty()))
  expectFalse(m.isSubset(of: initUnorderedSetOfCIntSubset()))
  expectTrue(m.isSubset(of: m))
  expectTrue(m.isSubset(of: initUnorderedSetOfCIntSuperset()))
  expectFalse(m.isSubset(of: initUnorderedSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isSubset(sequence)") {
  let m = initUnorderedSetOfCInt()

  expectFalse(m.isSubset(of: Array<CInt>()))
  expectFalse(m.isSubset(of: [CInt(2)]))
  expectFalse(m.isSubset(of: [CInt(2), 4]))
  expectTrue(m.isSubset(of: [CInt(2), 4, 6]))
  expectTrue(m.isSubset(of: [CInt(2), 4, 6, 8]))
  expectFalse(m.isSubset(of: [CInt(2), 6, 8]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isSubset(sequence).withDuplicates") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isSubset(of: [CInt(2), 4, 4, 6, 6, 6]))
  expectTrue(m.isSubset(of: [CInt(2), 2, 4, 6, 8, 8, 8]))
  expectFalse(m.isSubset(of: [CInt(2), 2, 6, 6, 8, 8]))
}

StdSetTestSuite.test("SetOfCInt.isStrictSubset(cxxset)") {
  let m = initSetOfCInt()

  expectFalse(m.isStrictSubset(of: initSetOfCIntEmpty()))
  expectFalse(m.isStrictSubset(of: initSetOfCIntSubset()))
  expectFalse(m.isStrictSubset(of: m))
  expectTrue(m.isStrictSubset(of: initSetOfCIntSuperset()))
  expectFalse(m.isStrictSubset(of: initSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("SetOfCInt.isStrictSubset(sequence)") {
  let m = initSetOfCInt()

  expectFalse(m.isStrictSubset(of: Array<CInt>()))
  expectFalse(m.isStrictSubset(of: [CInt(1)]))
  expectFalse(m.isStrictSubset(of: [CInt(1), 3]))
  expectFalse(m.isStrictSubset(of: [CInt(1), 3, 5]))
  expectFalse(m.isStrictSubset(of: [CInt(1), 3, 5, 3]))
  expectTrue(m.isStrictSubset(of: [CInt(1), 3, 5, 7]))
  expectTrue(m.isStrictSubset(of: [CInt(1), 3, 5, 7, 3]))
  expectFalse(m.isStrictSubset(of: [CInt(1), 5, 7]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isStrictSubset(cxxset)") {
  let m = initUnorderedSetOfCInt()

  expectFalse(m.isStrictSubset(of: initUnorderedSetOfCIntEmpty()))
  expectFalse(m.isStrictSubset(of: initUnorderedSetOfCIntSubset()))
  expectFalse(m.isStrictSubset(of: m))
  expectTrue(m.isStrictSubset(of: initUnorderedSetOfCIntSuperset()))
  expectFalse(m.isStrictSubset(of: initUnorderedSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isStrictSubset(sequence)") {
  let m = initUnorderedSetOfCInt()

  expectFalse(m.isStrictSubset(of: Array<CInt>()))
  expectFalse(m.isStrictSubset(of: [CInt(2)]))
  expectFalse(m.isStrictSubset(of: [CInt(2), 4]))
  expectFalse(m.isStrictSubset(of: [CInt(2), 4, 6]))
  expectFalse(m.isStrictSubset(of: [CInt(2), 4, 6, 4]))
  expectTrue(m.isStrictSubset(of: [CInt(2), 4, 6, 8]))
  expectTrue(m.isStrictSubset(of: [CInt(2), 4, 6, 8, 4]))
  expectFalse(m.isStrictSubset(of: [CInt(2), 6, 8]))
}

StdSetTestSuite.test("SetOfCInt.isSuperset(cxxset)") {
  let m = initSetOfCInt()

  expectTrue(m.isSuperset(of: initSetOfCIntEmpty()))
  expectTrue(m.isSuperset(of: initSetOfCIntSubset()))
  expectTrue(m.isSuperset(of: m))
  expectFalse(m.isSuperset(of: initSetOfCIntSuperset()))
  expectFalse(m.isSuperset(of: initSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("SetOfCInt.isSuperset(sequence)") {
  let m = initSetOfCInt()

  expectTrue(m.isSuperset(of: Array<CInt>()))
  expectTrue(m.isSuperset(of: [CInt(1)]))
  expectTrue(m.isSuperset(of: [CInt(1), 3]))
  expectTrue(m.isSuperset(of: [CInt(1), 3, 5]))
  expectFalse(m.isSuperset(of: [CInt(1), 3, 5, 7]))
  expectFalse(m.isSuperset(of: [CInt(1), 5, 7]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isSuperset(cxxset)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isSuperset(of: initUnorderedSetOfCIntEmpty()))
  expectTrue(m.isSuperset(of: initUnorderedSetOfCIntSubset()))
  expectTrue(m.isSuperset(of: m))
  expectFalse(m.isSuperset(of: initUnorderedSetOfCIntSuperset()))
  expectFalse(m.isSuperset(of: initUnorderedSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isSuperset(sequence)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isSuperset(of: Array<CInt>()))
  expectTrue(m.isSuperset(of: [CInt(2)]))
  expectTrue(m.isSuperset(of: [CInt(2), 4]))
  expectTrue(m.isSuperset(of: [CInt(2), 4, 6]))
  expectFalse(m.isSuperset(of: [CInt(2), 4, 6, 8]))
  expectFalse(m.isSuperset(of: [CInt(2), 6, 8]))
}

StdSetTestSuite.test("SetOfCInt.isStrictSuperset(cxxset)") {
  let m = initSetOfCInt()

  expectTrue(m.isStrictSuperset(of: initSetOfCIntEmpty()))
  expectTrue(m.isStrictSuperset(of: initSetOfCIntSubset()))
  expectFalse(m.isStrictSuperset(of: m))
  expectFalse(m.isStrictSuperset(of: initSetOfCIntSuperset()))
  expectFalse(m.isStrictSuperset(of: initSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("SetOfCInt.isStrictSuperset(sequence)") {
  let m = initSetOfCInt()

  expectTrue(m.isStrictSuperset(of: Array<CInt>()))
  expectTrue(m.isStrictSuperset(of: [CInt(1)]))
  expectTrue(m.isStrictSuperset(of: [CInt(1), 3]))
  expectFalse(m.isStrictSuperset(of: [CInt(1), 3, 5]))
  expectFalse(m.isStrictSuperset(of: [CInt(1), 3, 5, 3]))
  expectFalse(m.isStrictSuperset(of: [CInt(1), 3, 5, 7]))
  expectFalse(m.isStrictSuperset(of: [CInt(1), 3, 5, 7, 3]))
  expectFalse(m.isStrictSuperset(of: [CInt(1), 5, 7]))

  let empty = initSetOfCIntEmpty()
  expectFalse(empty.isStrictSuperset(of: Array<CInt>()))
  expectFalse(empty.isStrictSuperset(of: [CInt(1)]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isStrictSuperset(cxxset)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isStrictSuperset(of: initUnorderedSetOfCIntEmpty()))
  expectTrue(m.isStrictSuperset(of: initUnorderedSetOfCIntSubset()))
  expectFalse(m.isStrictSuperset(of: m))
  expectFalse(m.isStrictSuperset(of: initUnorderedSetOfCIntSuperset()))
  expectFalse(m.isStrictSuperset(of: initUnorderedSetOfCIntHasIntersection()))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isStrictSuperset(sequence)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isStrictSuperset(of: Array<CInt>()))
  expectTrue(m.isStrictSuperset(of: [CInt(2)]))
  expectTrue(m.isStrictSuperset(of: [CInt(2), 4]))
  expectFalse(m.isStrictSuperset(of: [CInt(2), 4, 6]))
  expectFalse(m.isStrictSuperset(of: [CInt(2), 4, 6, 4]))
  expectFalse(m.isStrictSuperset(of: [CInt(2), 4, 6, 8]))
  expectFalse(m.isStrictSuperset(of: [CInt(2), 4, 6, 8, 4]))
  expectFalse(m.isStrictSuperset(of: [CInt(2), 6, 8]))

  let empty = initUnorderedSetOfCIntEmpty()
  expectFalse(empty.isStrictSuperset(of: Array<CInt>()))
  expectFalse(empty.isStrictSuperset(of: [CInt(2)]))
}

StdSetTestSuite.test("SetOfCInt.isDisjoint(cxxset)") {
  let m = initSetOfCInt()

  expectTrue(m.isDisjoint(with: initSetOfCIntEmpty()))
  expectFalse(m.isDisjoint(with: initSetOfCIntSubset()))
  expectFalse(m.isDisjoint(with: m))
  expectFalse(m.isDisjoint(with: initSetOfCIntSuperset()))
  expectFalse(m.isDisjoint(with: initSetOfCIntHasIntersection()))
  expectTrue(m.isDisjoint(with: initSetOfCIntDisjoint()))
}

StdSetTestSuite.test("SetOfCInt.isDisjoint(sequence)") {
  let m = initSetOfCInt()

  expectTrue(m.isDisjoint(with: Array<CInt>()))
  expectFalse(m.isDisjoint(with: [CInt(1)]))
  expectFalse(m.isDisjoint(with: [CInt(1), 3]))
  expectFalse(m.isDisjoint(with: [CInt(1), 3, 5]))
  expectFalse(m.isDisjoint(with: [CInt(1), 3, 5, 3]))
  expectFalse(m.isDisjoint(with: [CInt(1), 3, 5, 7]))
  expectFalse(m.isDisjoint(with: [CInt(1), 3, 5, 7, 3]))
  expectFalse(m.isDisjoint(with: [CInt(1), 5, 7]))
  expectTrue(m.isDisjoint(with: [CInt(2), 4]))
  expectTrue(m.isDisjoint(with: [CInt(2), 4, 6]))
  expectTrue(m.isDisjoint(with: [CInt(2), 4, 6, 8]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isDisjoint(cxxset)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isDisjoint(with: initUnorderedSetOfCIntEmpty()))
  expectFalse(m.isDisjoint(with: initUnorderedSetOfCIntSubset()))
  expectFalse(m.isDisjoint(with: m))
  expectFalse(m.isDisjoint(with: initUnorderedSetOfCIntSuperset()))
  expectFalse(m.isDisjoint(with: initUnorderedSetOfCIntHasIntersection()))
  expectTrue(m.isDisjoint(with: initUnorderedSetOfCIntDisjoint()))
}

StdSetTestSuite.test("UnorderedSetOfCInt.isDisjoint(sequence)") {
  let m = initUnorderedSetOfCInt()

  expectTrue(m.isDisjoint(with: Array<CInt>()))
  expectFalse(m.isDisjoint(with: [CInt(2)]))
  expectFalse(m.isDisjoint(with: [CInt(2), 4]))
  expectFalse(m.isDisjoint(with: [CInt(2), 4, 6]))
  expectFalse(m.isDisjoint(with: [CInt(2), 4, 6, 4]))
  expectFalse(m.isDisjoint(with: [CInt(2), 4, 6, 8]))
  expectFalse(m.isDisjoint(with: [CInt(2), 4, 6, 8, 4]))
  expectFalse(m.isDisjoint(with: [CInt(2), 6, 8]))
  expectTrue(m.isDisjoint(with: [CInt(1), 3]))
  expectTrue(m.isDisjoint(with: [CInt(1), 3, 5]))
  expectTrue(m.isDisjoint(with: [CInt(1), 3, 5, 7]))
}

StdSetTestSuite.test("UnorderedSetOfCInt.symmetricDifference(cxxset)") {
  let m = initUnorderedSetOfCInt()
  let n = initUnorderedSetOfCInt2()
  let result = m.symmetricDifference(n)
  expectEqual(result.count, 3)
  expectTrue(result.contains(2))
  expectTrue(result.contains(6))
  expectTrue(result.contains(8))
}

StdSetTestSuite.test("UnorderedSetOfCInt.symmetricDifference(sequence)") {
  let m = initUnorderedSetOfCInt()
  let result = m.symmetricDifference([CInt(4), 8, 10])
  expectEqual(result.count, 4)
  expectTrue(result.contains(2))
  expectTrue(result.contains(6))
  expectTrue(result.contains(8))
  expectTrue(result.contains(10))
}

StdSetTestSuite.test("SetOfCInt.formSymmetricDifference(cxxset)") {
  var m = initSetOfCInt()
  let n = initSetOfCInt2()
  m.formSymmetricDifference(n)
  expectEqual(m.count, 3)
  expectTrue(m.contains(1))
  expectTrue(m.contains(5))
  expectTrue(m.contains(7))
}

StdSetTestSuite.test("SetOfCInt.formSymmetricDifference(sequence)") {
  var m = initSetOfCInt()
  m.formSymmetricDifference([CInt(3), 7, 9])
  expectEqual(m.count, 4)
  expectTrue(m.contains(1))
  expectTrue(m.contains(5))
  expectTrue(m.contains(7))
  expectTrue(m.contains(9))
}

StdSetTestSuite.test("SetOfCInt.formSymmetricDifference(sequence).withDuplicates") {
  var m = initSetOfCInt()
  m.formSymmetricDifference([CInt(3), 7, 7, 9])
  expectEqual(m.count, 4)
  expectTrue(m.contains(1))
  expectTrue(m.contains(5))
  expectTrue(m.contains(7))
  expectTrue(m.contains(9))
}

StdSetTestSuite.test("UnorderedSetOfCInt.formSymmetricDifference(cxxset)") {
  var m = initUnorderedSetOfCInt()
  let n = initUnorderedSetOfCInt2()
  m.formSymmetricDifference(n)
  expectEqual(m.count, 3)
  expectTrue(m.contains(2))
  expectTrue(m.contains(6))
  expectTrue(m.contains(8))
}

StdSetTestSuite.test("UnorderedSetOfCInt.formSymmetricDifference(sequence)") {
  var m = initUnorderedSetOfCInt()
  m.formSymmetricDifference([CInt(4), 8, 10])
  expectEqual(m.count, 4)
  expectTrue(m.contains(2))
  expectTrue(m.contains(6))
  expectTrue(m.contains(8))
  expectTrue(m.contains(10))
}

StdSetTestSuite.test("UnorderedSetOfCInt.formSymmetricDifference(sequence).withDuplicates") {
  var m = initUnorderedSetOfCInt()
  m.formSymmetricDifference([CInt(4), 8, 8, 10])
  expectEqual(m.count, 4)
  expectTrue(m.contains(2))
  expectTrue(m.contains(6))
  expectTrue(m.contains(8))
  expectTrue(m.contains(10))
}

StdSetTestSuite.test("SetOfCInt.symmetricDifference(sequence)") {
  let m = initSetOfCInt()
  let result = m.symmetricDifference([CInt(3), 7, 9])
  expectEqual(result.count, 4)
  expectTrue(result.contains(1))
  expectTrue(result.contains(5))
  expectTrue(result.contains(7))
  expectTrue(result.contains(9))
}

StdSetTestSuite.test("SetOfCInt.symmetricDifference(sequence).withDuplicates") {
  let m = initSetOfCInt()
  let result = m.symmetricDifference([CInt(3), 3, 7, 7, 7, 9, 9])
  expectEqual(result.count, 4)
  expectTrue(result.contains(1))
  expectTrue(result.contains(5))
  expectTrue(result.contains(7))
  expectTrue(result.contains(9))
}

runAllTests()
