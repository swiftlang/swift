// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: threading_none

// Exercise the metadata cache from multiple threads to shake out any
// concurrency bugs.

import StdlibUnittest

import SwiftPrivateThreadExtras

struct One {}
struct Two {}

struct Cat<T, U> {}

protocol Growable {}
extension Growable {
  func grow() -> (Growable, Growable) {
    return (Cat<Self, One>(), Cat<Self, Two>())
  }
}

extension One: Growable {}
extension Two: Growable {}
extension Cat: Growable {}

var ConcurrentMetadataTestSuite = TestSuite("ConcurrentMetadata")

ConcurrentMetadataTestSuite.test("ConcurrentMetadata") {
  let threadCount = 16
  let iterationCount = 10000

  func threadFunc() {
    var array: [Growable] = [One(), Two()]
    for i in 0..<iterationCount {
      // Each call to grow() creates a new generic metadata specialization which
      // will race with creating that same metadata on the other threads.
      let (a, b) = array[i].grow()
      array.append(a)
      array.append(b)
    }
  }

  let threadIDs = (0..<16).map { _ -> ThreadHandle in
    let (ret, threadID) = _stdlib_thread_create_block(threadFunc, ())
    expectEqual(0, ret)
    return threadID!
  }

  for threadID in threadIDs {
    let (ret, _) = _stdlib_thread_join(threadID, Void.self)
    expectEqual(0, ret)
  }
}

runAllTests()
