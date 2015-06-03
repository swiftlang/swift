// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

struct Buffer32 {
  var x0: UInt64 = 0
  var x1: UInt64 = 0
  var x2: UInt64 = 0
  var x3: UInt64 = 0
}

func foo() -> UInt64 {
  var buffer = Buffer32()
  var v0: UInt64 = 1
  var v1: UInt64 = 2
  var b: Bool = true
  return withUnsafeMutablePointer(&buffer) { bufferPtr in 
    bufferPtr.memory.x0 = 5
    bufferPtr.memory.x1 = v0
    bufferPtr.memory.x2 = v1
    bufferPtr.memory.x3 = b ? v0 : v1
    return bufferPtr.memory.x3
  }
}

// CHECK: 1
print(foo())
