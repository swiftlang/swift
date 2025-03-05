// RUN: %target-run-simple-swift(-enable-experimental-feature IsolatedConformances -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_IsolatedConformances
// UNSUPPORTED: back_deployment_runtime

protocol P {
  func f()
}

@MainActor
class MyClass: isolated P {
  func f() {
    print("MyClass.f()")

    // Make sure we're on the main actor.
    MainActor.assumeIsolated { }
  }
}

struct Wrapper<T> {
  var wrapped: T
}

extension Wrapper: P where T: P {
  func f() {
    print("Wrapper for ", terminator: "")
    wrapped.f()
  }
}

func tryCastToP(_ value: any Sendable) -> Bool {
  if let p = value as? any P {
    p.f()
    return true
  }

  print("Conformance did not match")
  return false
}

// CHECK: Testing on the main actor
// CHECK-NEXT: MyClass.f()
// CHECK-NEXT: Wrapper for MyClass.f()
print("Testing on the main actor")
let mc = MyClass()
let wrappedMC = Wrapper(wrapped: mc)
precondition(tryCastToP(mc))
precondition(tryCastToP(wrappedMC))

// CHECK: Testing a separate task on the main actor
// CHECK-NEXT: MyClass.f()
// CHECK-NEXT: Wrapper for MyClass.f()
print("Testing a separate task on the main actor")
await Task.detached { @MainActor in
  precondition(tryCastToP(mc))
  precondition(tryCastToP(wrappedMC))
}.value

// FIXME: Currently not handling the wrapper case appropriately, because
// we don't track whether we used an isolated conformance to satisfy another
// conformance at runtime.

// CHECK: Testing a separate task off the main actor
print("Testing a separate task off the main actor")
await Task.detached {
  if #available(SwiftStdlib 6.2, *) {
    precondition(!tryCastToP(mc))
  // precondition(!tryCastToP(wrappedMC))
  } else {
    print("Cast succeeds, but shouldn't")
    precondition(tryCastToP(mc))
  }
}.value

// Ensure that we access mc later
print(mc)
