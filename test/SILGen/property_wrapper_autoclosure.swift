// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@propertyWrapper
public class Autoclosure<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: @autoclosure () -> Value) {
    print("Property Wrapper init")
    self.wrappedValue = wrappedValue()
  }
}

struct S {
  init() { print("S.init") }
}

struct ValueHolder {
  @Autoclosure var value: S
}

// CHECK: Property Wrapper init
// CHECK-NEXT: S.init
_ = ValueHolder(value: S())
