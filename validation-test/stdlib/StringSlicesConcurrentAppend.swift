// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: stress_test

import StdlibUnittest
import SwiftPrivateThreadExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif


var StringTestSuite = TestSuite("String")

extension String {
  var capacity: Int {
    return _classify()._capacity
  }
}

// Swift.String used to hsve an optimization that allowed us to append to a
// shared string buffer.  However, as lock-free programming invariably does, it
// introduced a race condition [rdar://25398370 Data Race in StringBuffer.append
// (found by TSan)].
//
// These tests verify that it works correctly when two threads try to append to
// different non-shared strings that point to the same shared buffer.  They used
// to verify that the first append could succeed without reallocation even if
// the string was held by another thread, but that has been removed.  This could
// still be an effective thread-safety test, though.

enum ThreadID {
  case Primary
  case Secondary
}

var barrierVar: UnsafeMutablePointer<_stdlib_pthread_barrier_t>?
var sharedString: String = ""
var secondaryString: String = ""

func barrier() {
  var ret = _stdlib_pthread_barrier_wait(barrierVar!)
  expectTrue(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD)
}

func sliceConcurrentAppendThread(_ tid: ThreadID) {
  for i in 0..<100 {
    barrier()
    if tid == .Primary {
      // Get a fresh buffer.
      sharedString = ""
      sharedString.append("abc")
      sharedString.reserveCapacity(16)
      expectLE(16, sharedString.capacity)
    }

    barrier()

    // Get a private string.
    var privateString = sharedString

    barrier()

    // Append to the private string.
    if tid == .Primary {
      privateString.append("def")
    } else {
      privateString.append("ghi")
    }

    barrier()

    // Verify that contents look good.
    if tid == .Primary {
      expectEqual("abcdef", privateString)
    } else {
      expectEqual("abcghi", privateString)
    }
    expectEqual("abc", sharedString)

    // Verify that only one thread took ownership of the buffer.
    if tid == .Secondary {
      secondaryString = privateString
    }
    barrier()
  }
}

StringTestSuite.test("SliceConcurrentAppend") {
  barrierVar = UnsafeMutablePointer.allocate(capacity: 1)
  barrierVar!.initialize(to: _stdlib_pthread_barrier_t())
  var ret = _stdlib_pthread_barrier_init(barrierVar!, 2)
  expectEqual(0, ret)

  let (createRet1, tid1) = _stdlib_pthread_create_block(
    sliceConcurrentAppendThread, .Primary)
  let (createRet2, tid2) = _stdlib_pthread_create_block(
    sliceConcurrentAppendThread, .Secondary)

  expectEqual(0, createRet1)
  expectEqual(0, createRet2)

  let (joinRet1, _) = _stdlib_pthread_join(tid1!, Void.self)
  let (joinRet2, _) = _stdlib_pthread_join(tid2!, Void.self)

  expectEqual(0, joinRet1)
  expectEqual(0, joinRet2)

  ret = _stdlib_pthread_barrier_destroy(barrierVar!)
  expectEqual(0, ret)

  barrierVar!.deinitialize(count: 1)
  barrierVar!.deallocate()
}

runAllTests()
