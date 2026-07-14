// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -Xcc -std=c++20)
//
// REQUIRES: executable_test
// Ubuntu 20.04 ships with an old version of libstdc++, which does not provide
// std::contiguous_iterator_tag from C++20.
// UNSUPPORTED: LinuxDistribution=ubuntu-20.04
// UNSUPPORTED: LinuxDistribution=amzn-2

import StdlibUnittest
import CustomIterable

var CxxIterableTestSuite = TestSuite("CxxSequence")

CxxIterableTestSuite.test("SimpleNonCopyableSequence as Swift.Iterable") {
  guard #available(SwiftStdlib 6.4, *) else { return }

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

CxxIterableTestSuite.test("SimpleNonCopArrayWrapper as Swift.Iterable") {
  guard #available(SwiftStdlib 6.4, *) else { return }
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

CxxIterableTestSuite.test("ContiguousNonCopyableSequence as Swift.Iterable") {
  guard #available(SwiftStdlib 6.4, *) else { return }
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

CxxIterableTestSuite.test("ContiguousNonCopyableSequence as Swift.Iterable, with maximumCount") {
  guard #available(SwiftStdlib 6.4, *) else { return }
  let seq = ContiguousNonCopyableSequence()
  let arr : [Int32] = [10, 20, 30, 40, 50]

  var iterator = seq.makeBorrowingIterator()
  var innerCounter = 0
  var outerCounter = 0
  while true {
    let span = iterator.nextSpan(maxCount: 3)
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
