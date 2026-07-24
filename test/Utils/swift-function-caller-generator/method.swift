// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-function-caller-generator Test %t/test.swift > %t/out.swift
// RUN: %diff %t/out.swift %t/out.swift.expected

//--- test.swift
struct Foo {
  func nonMutating() -> Int

  mutating func mutatingMethod(x: Int)

  func unsafeMethod(p: UnsafeMutablePointer<Int>)
}

class Bar {
  class func classFunction(x: Int) -> Int
}

//--- out.swift.expected
import Test


extension Foo {
  func call_nonMutating() -> Int {
    return nonMutating()
  }
  mutating func call_mutatingMethod(x: Int) {
    return mutatingMethod(x: x)
  }
  func call_unsafeMethod(p: UnsafeMutablePointer<Int>) {
    return unsafe unsafeMethod(p: p)
  }
}

extension Bar {
  func call_classFunction(x: Int) -> Int {
    return Bar.classFunction(x: x)
  }
}
