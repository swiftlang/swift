// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -enable-experimental-feature BorrowingForLoop)

// REQUIRES: executable_test
// REQUIRES: swift_feature_BorrowingForLoop

import StdlibUnittest
import StdList
import CxxStdlib

var StdListTestSuite = TestSuite("StdList")

func getNumber(_ x: borrowing NonCopyable) -> Int32 {
    return x.number
}

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

StdListTestSuite.test("ListOfInt borrowing for loop") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    let constLst = makeListInt()
    expectEqual(constLst.size(), 3)
    expectFalse(constLst.empty())
    var counter = 0
    for el in constLst {
        expectEqual(el, arr[counter])
        counter += 1
    }
    expectEqual(counter, constLst.size())

    var mutLst = makeListInt()
    expectEqual(mutLst.size(), 3)
    expectFalse(mutLst.empty())
    counter = 0
    for el in mutLst {
        expectEqual(el, arr[counter])
        counter += 1
    }
    expectEqual(counter, mutLst.size())
}

StdListTestSuite.test("ListOfNonCopyable borrowing for loop") {
    guard #available(SwiftStdlib 6.4, *) else { return }
    let arr : [Int32] = [1, 2, 3]
    var constLst = makeListOfNonCopyable()
    expectEqual(constLst.size(), 3)
    expectFalse(constLst.empty())
    var counter = 0
    for el in constLst {
        expectEqual(getNumber(el), arr[counter])
        counter += 1
    }
    expectEqual(counter, constLst.size())

    var mutLst = makeListOfNonCopyable()
    expectEqual(mutLst.size(), 3)
    expectFalse(mutLst.empty())
    counter = 0
    for el in mutLst {
        expectEqual(getNumber(el), arr[counter])
        counter += 1
    }
    expectEqual(counter, mutLst.size())
}

runAllTests()
