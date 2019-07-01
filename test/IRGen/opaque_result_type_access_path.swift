// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: CPU=arm64 || CPU=x86_64

// Check that the IRGenMangler does not crash when mangling a conformance
// access path with an opaque result type as root.
// As a bonus, also do a runtime test to check that there is no miscompile.

protocol P {
  func get() -> Int
}

extension Int : P {
  func get() -> Int {
    return self
  }
}

struct X<T> {
  let tt: T
  init(_ t: T) {
    tt = t
  }
}

extension X : P where T : P {
  func get() -> Int {
    return tt.get()
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
func bar() -> some P {
  return 27
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
func foo() -> some P {
  return X(bar())
}

// CHECK: 27
if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) {
print(foo().get())
} else {
  print(27)
}
