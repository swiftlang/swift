// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import SwiftPrivatePthreadExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StringTestSuite = TestSuite("String")

extension String {
  var bufferID: UInt {
    return unsafeBitCast(_core._owner, UInt.self)
  }
  var capacityInBytes: Int {
    return _core.nativeBuffer!.capacity
  }
}

// Swift.String has an optimization that allows us to append to a shared string
// buffer.  Make sure that it works correctly when two threads try to append to
// different non-shared strings that point to the same shared buffer.

enum ThreadID {
  case Primary
  case Secondary
}

var barrierVar: UnsafeMutablePointer<_stdlib_pthread_barrier_t> = nil
var sharedString: String = ""
var secondaryString: String = ""

func barrier() {
  var ret = _stdlib_pthread_barrier_wait(barrierVar)
  expectTrue(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD)
}

func sliceConcurrentAppendThread(tid: ThreadID) {
  for i in 0..<100 {
    barrier()
    if tid == .Primary {
      // Get a fresh buffer.
      sharedString = ""
      sharedString.appendContentsOf("abc")
      sharedString.reserveCapacity(16)
      expectLE(16, sharedString.capacityInBytes)
    }

    barrier()

    // Get a private string.
    var privateString = sharedString

    barrier()

    // Append to the private string.
    if tid == .Primary {
      privateString.appendContentsOf("def")
    } else {
      privateString.appendContentsOf("ghi")
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
    if tid == .Primary {
      expectTrue(
        (privateString.bufferID == sharedString.bufferID) !=
          (secondaryString.bufferID == sharedString.bufferID))
    }
  }
}

StringTestSuite.test("SliceConcurrentAppend") {
  barrierVar = UnsafeMutablePointer.alloc(1)
  barrierVar.initialize(_stdlib_pthread_barrier_t())
  var ret = _stdlib_pthread_barrier_init(barrierVar, nil, 2)
  expectEqual(0, ret)

  let (createRet1, tid1) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Primary)
  let (createRet2, tid2) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Secondary)

  expectEqual(0, createRet1)
  expectEqual(0, createRet2)

  let (joinRet1, _) = _stdlib_pthread_join(tid1!, Void.self)
  let (joinRet2, _) = _stdlib_pthread_join(tid2!, Void.self)

  expectEqual(0, joinRet1)
  expectEqual(0, joinRet2)

  ret = _stdlib_pthread_barrier_destroy(barrierVar)
  expectEqual(0, ret)

  barrierVar.destroy()
  barrierVar.dealloc(1)
}

runAllTests()

