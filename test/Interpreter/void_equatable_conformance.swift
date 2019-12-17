// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

// CHECK: true
print(equals((), ()))

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func opaqueEquatableValue() -> some Equatable {
  ()
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  // CHECK: true
  print(opaqueEquatableValue() == opaqueEquatableValue())
}

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

// CHECK: true
print(Wrapper(value: ()) == Wrapper(value: ()))
