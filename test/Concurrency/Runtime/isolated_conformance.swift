// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// FIXME: WebAssembly doesn't currently have a good way to install the
// "isCurrentGlobalActor" hook on which this checking depends. Disable
// the test for the moment.
// UNSUPPORTED: wasm
// UNSUPPORTED: CPU=wasm32

protocol P {
  func f()
}

protocol Q {
  func g()
}

protocol R: Sendable {
  func h()
}

nonisolated class MyClass: @MainActor P {
  func f() {
    print("MyClass.f()")

    // Make sure we're on the main actor.
    MainActor.assumeIsolated { }
  }
}

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

extension MyClass: @SomeGlobalActor Q {
  @SomeGlobalActor func g() {
    print("MyClass.g()")

    // Make sure we're on this actor.
    SomeGlobalActor.shared.assumeIsolated { _ in }
  }
}

extension MyClass: nonisolated R {
  nonisolated func h() {
    print("MyClass.h()")
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

extension Wrapper: Q where T: Q {
  func g() {
    print("Wrapper for ", terminator: "")
    wrapped.g()
  }
}

extension Wrapper: R where T: R {
  func h() {
    print("Wrapper for ", terminator: "")
    wrapped.h()
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

@available(SwiftStdlib 5.9, *)
extension WrapMany: Q where repeat each T: Q {
  func g() {
    print("Wrapper for many")
  }
}

@available(SwiftStdlib 5.9, *)
extension WrapMany: R where repeat each T: R {
  func h() {
    print("Wrapper for many")
  }
}

extension Int: P, Q, R {
  func f() { }
  func g() { }
  func h() { }
}

extension String: P, Q, R {
  func f() { }
  func g() { }
  func h() { }
}

func tryCastToP(_ value: any Sendable) -> Bool {
  if let p = value as? any P {
    p.f()
    return true
  }

  print("Conformance did not match")
  return false
}

func tryCastToPAndR(_ value: any Sendable) -> Bool {
  if let p = value as? any P & R {
    p.f()
    return true
  }

  print("Conformance did not match")
  return false
}

func tryCastToQ(_ value: any Sendable) -> Bool {
  if let q = value as? any Q {
    q.g()
    return true
  }

  print("Conformance did not match")
  return false
}

// CHECK: Testing on the main actor
// CHECK-NEXT: MyClass.f()
// CHECK-NEXT: Wrapper for MyClass.f()
print("Testing on the main actor")
nonisolated let mc = MyClass()
nonisolated let wrappedMC = Wrapper(wrapped: mc)
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

  // Cannot cast to P & R because the conformance to P is isolated, but R
  // is Sendable.
  precondition(!tryCastToPAndR(mc))
  precondition(!tryCastToPAndR(wrappedMC))

  if #available(SwiftStdlib 5.9, *) {
    let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
    precondition(tryCastToP(wrappedMany))
  }

}.value

// CHECK: Testing a separate task on a different global actor
// CHECK-NEXT: MyClass.g()
// CHECK-NEXT: Wrapper for MyClass.g()
print("Testing a separate task on a different global actor")
await Task.detached { @SomeGlobalActor in
  precondition(tryCastToQ(mc))
  precondition(tryCastToQ(wrappedMC))

  // Cannot cast to P & R because the conformance to P is isolated, but R
  // is Sendable.
  precondition(!tryCastToPAndR(mc))
  precondition(!tryCastToPAndR(wrappedMC))

  if #available(SwiftStdlib 5.9, *) {
    let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
    precondition(tryCastToQ(wrappedMany))
  }

  // Not on the main actor any more.
  precondition(!tryCastToP(mc))
  precondition(!tryCastToP(wrappedMC))
}.value

// CHECK: Testing a separate task off the main actor
print("Testing a separate task off the main actor")
await Task.detached {
  if #available(SwiftStdlib 6.2, *) {
    precondition(!tryCastToP(mc))
    precondition(!tryCastToP(wrappedMC))

    precondition(!tryCastToQ(mc))
    precondition(!tryCastToQ(wrappedMC))

    let wrappedMany = WrapMany(wrapped: (17, mc, "Pack"))
    precondition(!tryCastToP(wrappedMany))
  } else {
    print("Cast succeeds, but shouldn't")
  }
}.value


// Ensure that we access mc later
print(mc)
