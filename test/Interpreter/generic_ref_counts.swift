// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: OS=openbsd

import Swift

// Regression test for <rdar://problem/16119895>.

struct Generic<T> {
  typealias Storage = ManagedBuffer<Int,T>

  init() {
    buffer = ManagedBufferPointer(
      bufferClass: Storage.self, minimumCapacity: 0) { _,_ in 0 }
  }

  mutating func isUniqueReference() -> Bool {
    return buffer.isUniqueReference()
  }
  
  var buffer: ManagedBufferPointer<Int, T>
}
func g0() {
  var x = Generic<Int>()
  // CHECK: true
  print(x.isUniqueReference())
  // CHECK-NEXT: true
  print(x.buffer.isUniqueReference())
}
g0()


struct NonGeneric {
  typealias T = Int
  typealias Storage = ManagedBuffer<Int,T>

  init() {
    buffer = ManagedBufferPointer(
      bufferClass: Storage.self, minimumCapacity: 0) { _,_ in 0 }
  }

  mutating func isUniqueReference() -> Bool {
    return buffer.isUniqueReference()
  }
  
  var buffer: ManagedBufferPointer<Int, T>
}
func g1() {
  var x = NonGeneric()
  // CHECK-NEXT: true
  print(x.isUniqueReference())
  // CHECK-NEXT: true
  print(x.buffer.isUniqueReference())
}
g1()
