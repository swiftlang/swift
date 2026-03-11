// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// Test iterating through a list using the borrowing iterators (currently behind a feature flag).
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -enable-experimental-feature BorrowingForLoop -DBORROWING_ITERATOR_PROTOCOL)

// REQUIRES: executable_test
// REQUIRES: swift_feature_BorrowingForLoop

import StdlibUnittest
import StdList
import CxxStdlib

var StdListTestSuite = TestSuite("StdList")

func getNumber(_ x: borrowing NonCopyable) -> Int32 {
    return x.number
}

// FIXME https://github.com/swiftlang/swift/issues/87260
// Currently, `Sequence` doesn't conform to `BorrowingSequence`, which means that types like `Range` don't
// automatically conform to `BorrowingSequence`. When we enable the experimental feature `BorrowingForLoop`,
// all for-in loops use the new borrowing iterators, which means that all range iterations will be invalid.
#if !BORROWING_ITERATOR_PROTOCOL

StdListTestSuite.test("ListOfInt conforms to CxxBorrowingSequence") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    let lst = makeListInt()
    expectEqual(lst.size(), 3)
    expectFalse(lst.empty())

    var iterator : CxxBorrowingIterator<List> = lst.makeBorrowingIterator()
    var counter = 0
    while true {
        var span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            expectEqual(span[i], arr[counter])
            counter += 1
        }
    }
    expectEqual(counter, lst.size())
}

StdListTestSuite.test("ListOfNonCopyable conforms to CxxBorrowingSequence") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    var lst = makeListOfNonCopyable()
    expectEqual(lst.size(), 3)
    expectFalse(lst.empty())

    var iterator = lst.makeBorrowingIterator()
    var counter = 0
    while true {
        var span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            expectEqual(getNumber(span[i]), arr[counter])
            counter += 1
        }
    }
    expectEqual(counter, lst.size())
}

#else // !BORROWING_ITERATOR_PROTOCOL

StdListTestSuite.test("ListOfInt borrowing for loop") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    let lst = makeListInt()
    expectEqual(lst.size(), 3)
    expectFalse(lst.empty())
    var counter = 0
    for el in lst {
        expectEqual(el, arr[counter])
        counter += 1
    }
    expectEqual(counter, lst.size())
}

StdListTestSuite.test("ListOfNonCopyable borrowing for loop") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    var lst = makeListOfNonCopyable()
    expectEqual(lst.size(), 3)
    expectFalse(lst.empty())
    var counter = 0
    for el in lst {
        expectEqual(getNumber(el), arr[counter])
        counter += 1
    }
    expectEqual(counter, lst.size())
}

#endif // !BORROWING_ITERATOR_PROTOCOL

runAllTests()
