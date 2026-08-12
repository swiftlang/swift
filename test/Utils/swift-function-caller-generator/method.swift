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

extension Foo {
  func extensionMethod(x: Int) -> Int

  mutating func mutatingExtensionMethod(p: UnsafeMutablePointer<Int>)
}

//--- out.swift.expected
import Test


extension Foo {
  func call_nonMutating_Foo() -> Int {
    return nonMutating()
  }
  mutating func call_mutatingMethod_Foo(x: Int) {
    return mutatingMethod(x: x)
  }
  func call_unsafeMethod_Foo(p: UnsafeMutablePointer<Int>) {
    return unsafe unsafeMethod(p: p)
  }
}

extension Bar {
  final func call_classFunction_Bar_classmethod(x: Int) -> Int {
    return Bar.classFunction(x: x)
  }
}

extension Foo {
  func call_extensionMethod_Foo(x: Int) -> Int {
    return extensionMethod(x: x)
  }
  mutating func call_mutatingExtensionMethod_Foo(p: UnsafeMutablePointer<Int>) {
    return unsafe mutatingExtensionMethod(p: p)
  }
}
