// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import SwiftPrivatePthreadExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

// Swift.String has an optimization that allows us to append to a shared string
// buffer.  Make sure that it works correctly when two threads try to append to
// different non-shared strings that point to the same shared buffer.

var StringTestSuite = TestSuite("String")

// Specific to test:
// We need to know what the internal ID of the buffer is so we can check that only
// one thread owns a buffer.
//
// TODO: explain why we're looking for a specific capacity.
extension String {
  var bufferID: UInt {
    return unsafeBitCast(_core._owner, UInt.self)
  }
  var capacityInBytes: Int {
    return _core.nativeBuffer!.capacity
  }
}

// OwnerThread is the thread we expect to end up owning sharedString
// OtherThread is the thread we expect to *not* own sharedString
enum ThreadID {
  case OwnerThread
  case OtherThread
}

var barrierVar: UnsafeMutablePointer<_stdlib_pthread_barrier_t> = nil
var sharedString: String = ""
var otherThreadString: String = ""

func barrier() {
  var ret = _stdlib_pthread_barrier_wait(barrierVar)
  expectTrue(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD)
}

func sliceConcurrentAppendThread(tid: ThreadID) {
  for i in 0..<100 {
    barrier()
    if tid == .OwnerThread {
      // Get a fresh buffer.
      sharedString = ""
      sharedString.appendContentsOf("abc")
      sharedString.reserveCapacity(16)
      expectLE(16, sharedString.capacityInBytes)
    }

    barrier()

    // Get a private string.
    // we *should* be able to change this as we wish without modifying the underlying string.
    var privateString = sharedString

    barrier()

    // Append to the private string.
    // This should invoke a deep copy somewhere along the line,
    // but NOT change the underlying sharedString buffer.
    if tid == .OwnerThread {
      privateString.appendContentsOf("def")
    } else {
      privateString.appendContentsOf("ghi")
    }

    barrier()

    // Verify that contents look good.
    // if our current thread is the OwnerThread, it should be "abcdef"
    // if our current thread is NOT OwnerThread, it should be "abcghi"
    // However, sharedString should NOT be changed, and should be "abc" no matter what.
    if tid == .OwnerThread {
      expectEqual("abcdef", privateString)
    } else {
      expectEqual("abcghi", privateString)
    }
    
    // sharedString should not have been touched.
    expectEqual("abc", sharedString)

    // Verify that only one thread took ownership of the buffer.
    if tid == .OtherThread {
      otherThreadString = privateString
    }
    barrier()
    if tid == .OwnerThread {
      // We expect that 
      // privateString.bufferID == sharedString.bufferID
      //  - OR - 
      // otherThreadString.bufferID == sharedString.bufferID
      //
      // but not both. 
      expectTrue(
        (privateString.bufferID == sharedString.bufferID) !=
          (otherThreadString.bufferID == sharedString.bufferID))
    }
  }
}

StringTestSuite.test("SliceConcurrentAppend") {
  barrierVar = UnsafeMutablePointer.alloc(1)
  barrierVar.initialize(_stdlib_pthread_barrier_t())
  var ret = _stdlib_pthread_barrier_init(barrierVar, nil, 2)
  expectEqual(0, ret)

  let (createRet1, tid1) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Leader)
  let (createRet2, tid2) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Follower)

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

