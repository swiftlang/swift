// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift

// Regression test for <rdar://problem/16119895>.

struct Generic<T> {
  typealias Storage = _HeapBufferStorage<Int,T>

  init() {
    buffer = _HeapBuffer(Storage.self, 0, 0)
  }

  mutating func isUniquelyReferenced() -> Bool {
    return buffer.isUniquelyReferenced()
  }
  
  var buffer: _HeapBuffer<Int, T>
}
func g0() {
  var x = Generic<Int>()
  // CHECK: true
  print(x.isUniquelyReferenced())
  // CHECK-NEXT: true
  print(x.buffer.isUniquelyReferenced())
}
g0()


struct NonGeneric {
  typealias T = Int
  typealias Storage = _HeapBufferStorage<Int,T>

  init() {
    buffer = _HeapBuffer(Storage.self, 0, 0)
  }

  mutating func isUniquelyReferenced() -> Bool {
    return buffer.isUniquelyReferenced()
  }
  
  var buffer: _HeapBuffer<Int, T>
}
func g1() {
  var x = NonGeneric()
  // CHECK-NEXT: true
  print(x.isUniquelyReferenced())
  // CHECK-NEXT: true
  print(x.buffer.isUniquelyReferenced())
}
g1()
