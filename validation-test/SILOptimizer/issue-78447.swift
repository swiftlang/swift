// RUN: %target-build-swift %s -I %S/Inputs

import Issue_78447_C

func foo<T> (ptr: UnsafePointer<T>?) -> String? {
  return nil
}

class A {
  internal let ptr: xmlNsPtr
  internal init(ptr: xmlNsPtr) {
    self.ptr = ptr
  }
}

class B : A {
  func bar() -> String? {
    return foo(ptr: ptr.pointee.prefix)
  }
}
