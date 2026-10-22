// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O -g) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_OnceFunctions

// Patterns that adopting exactly-once and at-most-once closure parameters in
// library APIs runs into.

final class Tracked {
  let value: Int
  init(_ value: Int) { self.value = value }
  deinit { print("Tracked(\(value)) deinit") }
}

struct Failure: Error {}

func withOnce<R>(_ body: @once () -> R) -> R { body() }

func withAtMostOnce<R>(_ body: @callable(once) () -> R) -> R? {
  Bool.random() ? body() : nil
}

func withOnceRethrows<R>(_ body: @once () throws -> R) rethrows -> R {
  try body()
}

func forwardOnceRethrows<R>(_ body: @once () throws -> R) rethrows -> R {
  try withOnceRethrows(body)
}

@_transparent
func and(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () throws -> Bool
) rethrows -> Bool {
  lhs ? try rhs() : false
}

// A function value that needs a reabstraction thunk to become exactly-once.
func passFunctionValue(_ f: () -> Int) -> Int {
  withOnce(f)
}

// A closure with execution semantics that reads a property without a getter.
func readPointee(_ pointer: UnsafeMutablePointer<Int>) -> Int {
  withOnce { pointer.pointee }
}

// A closure with execution semantics that captures a `borrowing` parameter.
func captureBorrowing(_ tracked: borrowing Tracked) -> Int {
  withOnce { tracked.value }
}

// A ~Escapable value captured by an exactly-once closure.
@available(SwiftStdlib 6.2, *)
func captureSpan(_ span: Span<Int>) -> Int {
  withOnce { span.count }
}

// An exactly-once closure passed to an always-inlined function, like
// `withExtendedLifetime(_:_:)`.
@inline(__always)
func withExtended<T, R>(_ x: borrowing T, _ body: @once () -> R) -> R {
  defer { _fixLifetime(x) }
  return body()
}

// An at-most-once autoclosure passed to a transparent function, like
// `precondition(_:_:file:line:)`.
@_transparent
func check(_ condition: @autoclosure @callable(once) () -> Bool) {
  if !condition() { fatalError() }
}

// A closure with execution semantics that is called on one path and handed to
// another function on the other path.
func callOrForward<R>(_ c: Bool, _ body: @once () -> R) -> R {
  if c { return body() }
  return withOnce(body)
}

// An at-most-once autoclosure inlined into generic code.
func inlinedAutoclosure<T: BinaryInteger>(_ t: T, _ tracked: Tracked) -> Bool {
  and(t > 0, tracked.value > 0)
}

do {
  let tracked = Tracked(1)
  // CHECK: 1
  print(passFunctionValue { tracked.value })
  // CHECK-NEXT: 1
  print(withAtMostOnce { tracked.value } ?? 1)
  // CHECK-NEXT: 1
  print(forwardOnceRethrows { tracked.value })
  do {
    _ = try forwardOnceRethrows { () throws -> Int in throw Failure() }
  } catch {
    // CHECK-NEXT: caught
    print("caught")
  }
  // CHECK-NEXT: 1
  print(captureBorrowing(tracked))
  // CHECK-NEXT: true false
  print(inlinedAutoclosure(1, tracked), inlinedAutoclosure(-1, tracked))
  // CHECK-NEXT: 1
  print(withExtended(tracked) { tracked.value })
  check(tracked.value > 0)
  // CHECK-NEXT: 1 1
  print(callOrForward(true) { tracked.value }, callOrForward(false) { tracked.value })
  // CHECK-NEXT: 1 2
  print(tracked.value, Optional<Int>.none.map { $0 + tracked.value } ?? 2)
}
// CHECK-NEXT: Tracked(1) deinit

do {
  var value = 42
  // CHECK-NEXT: 42
  print(readPointee(&value))
}

if #available(SwiftStdlib 6.2, *) {
  let array = [1, 2, 3]
  // CHECK-NEXT: 3
  print(captureSpan(array.span))
} else {
  print(3)
}
