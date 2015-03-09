// RUN: %target-run-simple-swift

import StdlibUnittest
import SwiftPrivatePthreadExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

var StringTestSuite = TestSuite("String")

extension String {
  var bufferID: UWord {
    return unsafeBitCast(_core._owner, UWord.self)
  }
  var capacityInBytes: Int {
    return _core.nativeBuffer!.capacity
  }
}

// Swift.String has an optimization that allows us to append to a shared string
// buffer.  Make sure that it works correctly when two threads try to append to
// different non-shared strings that point to the same shared buffer.

enum ThreadID {
  case Master
  case Slave
}

var barrierVar: UnsafeMutablePointer<_stdlib_pthread_barrier_t> = nil
var sharedString: String = ""
var slaveString: String = ""

func barrier() {
  var ret = _stdlib_pthread_barrier_wait(barrierVar)
  expectTrue(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD)
}

func sliceConcurrentAppendThread(tid: ThreadID) {
  for i in 0..<100 {
    barrier()
    if tid == .Master {
      // Get a fresh buffer.
      sharedString = ""
      sharedString.extend("abc")
      sharedString.reserveCapacity(16)
      expectLE(16, sharedString.capacityInBytes)
    }

    barrier()

    // Get a private string.
    var privateString = sharedString

    barrier()

    // Append to the private string.
    if tid == .Master {
      privateString.extend("def")
    } else {
      privateString.extend("ghi")
    }

    barrier()

    // Verify that contents look good.
    if tid == .Master {
      expectEqual("abcdef", privateString)
    } else {
      expectEqual("abcghi", privateString)
    }
    expectEqual("abc", sharedString)

    // Verify that only one thread took ownership of the buffer.
    if tid == .Slave {
      slaveString = privateString
    }
    barrier()
    if tid == .Master {
      expectTrue(
        (privateString.bufferID == sharedString.bufferID) !=
          (slaveString.bufferID == sharedString.bufferID))
    }
  }
}

StringTestSuite.test("SliceConcurrentAppend") {
  barrierVar = UnsafeMutablePointer.alloc(1)
  barrierVar.initialize(_stdlib_pthread_barrier_t())
  var ret = _stdlib_pthread_barrier_init(barrierVar, nil, 2)
  expectEqual(0, ret)

  let (createRet1, tid1) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Master)
  let (createRet2, tid2) = _stdlib_pthread_create_block(
    nil, sliceConcurrentAppendThread, .Slave)

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

