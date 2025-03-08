// RUN: %target-run-simple-swift(-enable-experimental-feature IsolatedConformances -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_IsolatedConformances
// UNSUPPORTED: back_deployment_runtime

// FIXME: WebAssembly doesn't currently have a good way to install the
// "isCurrentGlobalActor" hook on which this checking depends. Disable
// the test for the moment.
// UNSUPPORTED: wasm
// UNSUPPORTED: CPU=wasm32

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

@available(SwiftStdlib 5.9, *)
struct WrapMany<each T> {
  var wrapped: (repeat each T)
}

@available(SwiftStdlib 5.9, *)
extension WrapMany: P where repeat each T: P {
  func f() {
    print("Wrapper for many")
  }
}

extension Int: P {
  func f() { }
}

extension String: P {
  func f() { }
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

if #available(SwiftStdlib 5.9, *) {
  let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
  precondition(tryCastToP(wrappedMany))
}

// CHECK: Testing a separate task on the main actor
// CHECK-NEXT: MyClass.f()
// CHECK-NEXT: Wrapper for MyClass.f()
print("Testing a separate task on the main actor")
await Task.detached { @MainActor in
  precondition(tryCastToP(mc))
  precondition(tryCastToP(wrappedMC))

  if #available(SwiftStdlib 5.9, *) {
    let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
    precondition(tryCastToP(wrappedMany))
  }

}.value

// CHECK: Testing a separate task off the main actor
print("Testing a separate task off the main actor")
await Task.detached {
  if #available(SwiftStdlib 6.2, *) {
    precondition(!tryCastToP(mc))
    precondition(!tryCastToP(wrappedMC))

    let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
    precondition(!tryCastToP(wrappedMany))
  } else {
    print("Cast succeeds, but shouldn't")
    precondition(tryCastToP(mc))
    precondition(tryCastToP(wrappedMC))

    if #available(SwiftStdlib 5.9, *) {
      let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
      precondition(tryCastToP(wrappedMany))
    }
  }
}.value

// Ensure that we access mc later
print(mc)
