// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -I %swift_src_root/lib/ClangImporter/SwiftBridging -Xcc -std=c++20)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu || OS=freebsd

import StdlibUnittest
import CustomBorrowingSequence

var CxxBorrowingSequenceTestSuite = TestSuite("CxxSequence")

CxxBorrowingSequenceTestSuite.test("SimpleNonCopyableSequence as Swift.BorrowingSequence") {
  guard #available(SwiftStdlib 6.3, *) else { return }

  let seq = SimpleNonCopyableSequence()
  let arr : [Int32] = [2, 3, 4, 5]

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
  guard #available(SwiftStdlib 6.3, *) else { return }
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

CxxBorrowingSequenceTestSuite.test("ContiguousNonCopyableSequence as Swift.BorrowingSequence") {
  guard #available(SwiftStdlib 6.3, *) else { return }
  let seq = ContiguousNonCopyableSequence()
  let arr : [Int32] = [10, 20, 30, 40, 50]

  var iterator = seq.makeBorrowingIterator()
  var innerCounter = 0
  var outerCounter = 0
  while true {
      let span = iterator.nextSpan()
      if (span.count == 0) { break }
      expectEqual(span.count, 5)
      for i in 0..<span.count {
          expectEqual(span[i], arr[innerCounter])
          innerCounter += 1
      }
      outerCounter += 1
  }
  expectEqual(innerCounter, 5)
  expectEqual(outerCounter, 1)
}

CxxBorrowingSequenceTestSuite.test("ContiguousNonCopyableSequence as Swift.BorrowingSequence, with maximumCount") {
  guard #available(SwiftStdlib 6.3, *) else { return }
  let seq = ContiguousNonCopyableSequence()
  let arr : [Int32] = [10, 20, 30, 40, 50]

  var iterator = seq.makeBorrowingIterator()
  var innerCounter = 0
  var outerCounter = 0
  while true {
      let span = iterator.nextSpan(maximumCount: 3)
      if (span.count == 0) { break }
      for i in 0..<span.count {
          expectEqual(span[i], arr[innerCounter])
          innerCounter += 1
      }
      outerCounter += 1
  }
  expectEqual(innerCounter, 5)
  expectEqual(outerCounter, 2)
}

runAllTests()
