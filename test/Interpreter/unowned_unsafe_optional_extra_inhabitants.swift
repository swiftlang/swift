// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// REQUIRES: executable_test

// The value witnesses of a struct whose extra inhabitants come from an
// optional unowned(unsafe) reference must agree with the inline enum code
// on which bit patterns are valid: a nil reference is a valid payload.
// https://github.com/swiftlang/swift/issues/92903

final class Item {}
protocol P: AnyObject {}

struct E { unowned(unsafe) var item: Item? }
struct F { unowned(unsafe) var p: P? }

@inline(never) @_optimize(none)
func isNil<T>(_ x: T?) -> Bool { x == nil }

@inline(never) @_optimize(none)
func makeNil<T>(_: T.Type) -> T? { nil }

@inline(never) @_optimize(none)
func makeNilNil<T>(_: T.Type) -> T?? { .some(nil) }

func test<T>(_ name: String, _ value: T) {
  print(name)
  print(isNil(Optional(value)))
  print(makeNil(T.self) == nil)
  let nn: T?? = makeNilNil(T.self)
  print(nn != nil && nn! == nil)

  var c = ContiguousArray([value, value, value])
  _ = c.removeLast()
  print(c.count)

  var n = 0
  for _ in [value] {
    n += 1
    if n > 1 { break }
  }
  print(n)
}

// CHECK-LABEL: E
// CHECK-NEXT: false
// CHECK-NEXT: true
// CHECK-NEXT: true
// CHECK-NEXT: 2
// CHECK-NEXT: 1
test("E", E(item: nil))

// CHECK-LABEL: F
// CHECK-NEXT: false
// CHECK-NEXT: true
// CHECK-NEXT: true
// CHECK-NEXT: 2
// CHECK-NEXT: 1
test("F", F(p: nil))
