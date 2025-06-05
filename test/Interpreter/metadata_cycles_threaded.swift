// RUN: %target-run-simple-swift(%import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: libdispatch
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@_optimize(none) @inline(never) func forceTypeInstantiation(_: Any.Type) {}

struct AnyFoo<T, U> {
  var thing: U
}

struct S<T> {
  var thing: T
  var next: AnyFoo<S, T>?
}

// We want to ensure that the runtime handles legal metadata cycles when threads
// race to instantiate the cycle. We have a cycle between S and AnyFoo, but it's
// resolvable because AnyFoo doesn't depend on S's layout. This tests a fix for
// a bug where the runtime's cycle detection could be overeager when multiple
// threads raced, and flag a legal.
//
// Since this is a multithreading test, failures are probabilistic and each type
// can only be tested once. The recursiveTry construct generates a large number
// of distinct types so we can do many tests.
func tryWithType<T>(_ t: T.Type) {
  DispatchQueue.concurrentPerform(iterations: 5) { n in
    forceTypeInstantiation(AnyFoo<S<T>, T>?.self)
  }
}

struct One<T> {}
struct Two<T> {}

func recursiveTry<T>(_ t: T.Type, depth: Int = 0) {
  if depth > 10 { return }
  tryWithType(T.self)
  recursiveTry(One<T>.self, depth: depth + 1)
  recursiveTry(Two<T>.self, depth: depth + 1)
}

recursiveTry(Int.self)
