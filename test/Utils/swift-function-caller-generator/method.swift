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

//--- out.swift.expected
import Test


func call_nonMutating(_ self: Foo) -> Int {
  return self.nonMutating()
}
func call_mutatingMethod(_ self: inout Foo, x: Int) {
  return self.mutatingMethod(x: x)
}

func call_unsafeMethod(_ self: Foo, p: UnsafeMutablePointer<Int>) {
  return unsafe self.unsafeMethod(p: p)
}
