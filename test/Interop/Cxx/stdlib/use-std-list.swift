// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import StdlibUnittest
import StdList
import CxxStdlib

var StdListTestSuite = TestSuite("StdList")

func getNumber(_ x: borrowing NonCopyable) -> Int32 {
    return x.number
}

StdListTestSuite.test("ListOfInt conforms to CxxBorrowingSequence") {
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

runAllTests()
