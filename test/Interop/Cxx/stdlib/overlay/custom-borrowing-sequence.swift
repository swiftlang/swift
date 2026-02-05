// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -I %swift_src_root/lib/ClangImporter/SwiftBridging)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu || OS=freebsd

import StdlibUnittest
import CustomBorrowingSequence

var CxxBorrowingSequenceTestSuite = TestSuite("CxxSequence")

CxxBorrowingSequenceTestSuite.test("SimpleNonCopyableSequence as Swift.BorrowingSequence") {
  let seq = SimpleNonCopyableSequence()
  let arr : [Int32] = [2, 3, 4, 5] // TODO should be [1, 2, 3, 4]

  var iterator = seq.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            expectEqual(span[i], arr[counter])
            counter += 1
        }
    }
    expectEqual(counter, 4)
}

CxxBorrowingSequenceTestSuite.test("SimpleNonCopArrayWrapper as Swift.BorrowingSequence") {
  let seq = SimpleNonCopArrayWrapper()
  let arr : [Int32] = [10, 20, 30, 40, 50]

  var iterator = seq.makeBorrowingIterator()
    var counter = 0
    while true {
        let span = iterator.nextSpan()
        if (span.count == 0) { break }
        for i in 0..<span.count {
            expectEqual(span[i].number, arr[counter])
            counter += 1
        }
    }
    expectEqual(counter, 5)
}

runAllTests()
