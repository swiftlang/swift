// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

protocol MyProtocol {}

func returnsClass1() -> some MyProtocol {
  class MyClass1: MyProtocol {}
  return MyClass1()
}

var returnsClass2: some MyProtocol {
  class MyClass2: MyProtocol {}
  return MyClass2()
}

print(returnsClass1())
// CHECK: a.(unknown context at ${{[0-9a-z]+}}).(unknown context at ${{[0-9a-z]+}}).MyClass1

print(returnsClass2)
// CHECK: a.(unknown context at ${{[0-9a-z]+}}).(unknown context at ${{[0-9a-z]+}}).MyClass2
