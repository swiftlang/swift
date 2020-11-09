// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

protocol MyProtocol {}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func returnsClass1() -> some MyProtocol {
  class MyClass1: MyProtocol {}
  return MyClass1()
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
struct Outer {
  static var returnsClass2: some MyProtocol {
    class MyClass2: MyProtocol {}
    return MyClass2()
  }
}

// CHECK: a.(unknown context at ${{[0-9a-z]+}}).(unknown context at ${{[0-9a-z]+}}).MyClass1
// CHECK: a.Outer.(unknown context at ${{[0-9a-z]+}}).(unknown context at ${{[0-9a-z]+}}).MyClass2
if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  print(returnsClass1())
  print(Outer.returnsClass2)
} else {
  // Make FileCheck happy if this test runs on an older OS.
  print("a.(unknown context at $0).(unknown context at $0).MyClass1")
  print("a.Outer.(unknown context at $0).(unknown context at $0).MyClass2")
}
