// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -enable-experimental-feature BorrowingSequence)
// Test iterating through a list using the borrowing iterators (currently behind a feature flag).

// REQUIRES: executable_test
// REQUIRES: swift_feature_BorrowingSequence

import StdlibUnittest
import StdList
import CxxStdlib

var StdListTestSuite = TestSuite("StdList")

func getNumber(_ x: borrowing NonCopyable) -> Int32 {
    return x.number
}

@available(SwiftStdlib 6.4, *)
func testIterable<I: Iterable>(
  _ iterable: borrowing I,
  _ getNumber: (borrowing I.Element) -> Int32
) where I.Element: ~Copyable, I: ~Copyable & ~Escapable, I.Failure == Never {
    var iterator = iterable.makeBorrowingIterator()
    var counter = 0
    while true {
        var span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            counter += 1
            expectEqual(getNumber(span[i]), Int32(counter))
        }
    }
    expectEqual(counter, 3)
}

@available(SwiftStdlib 6.4, *)
func testCxxIterable<I: CxxIterable>(
  _ iterable: borrowing I,
  _ getNumber: (borrowing I.Element) -> Int32
) where I.Element: ~Copyable, I: ~Copyable & ~Escapable {
    var iterator = iterable.makeBorrowingIterator()
    var counter = 0
    while true {
        var span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            counter += 1
            expectEqual(getNumber(span[i]), Int32(counter))
        }
    }
    expectEqual(counter, 3)
}

StdListTestSuite.test("ListOfInt conforms to CxxIterable") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let lst = makeListInt()
    testIterable(lst, { $0 })
    testCxxIterable(lst, { $0 })
}

StdListTestSuite.test("ListOfNonCopyable conforms to CxxIterable") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    var lst = makeListOfNonCopyable()
    testIterable(lst, getNumber)
    testCxxIterable(lst, getNumber)
}

runAllTests()
