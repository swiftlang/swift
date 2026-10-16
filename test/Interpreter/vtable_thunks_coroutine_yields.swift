// RUN: %target-run-simple-swift(-Xfrontend -disable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors) | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

// Reads through the base class must see the base's yield type, even when the
// override yields a different one.

final class Tracked {
  let name: String
  init(_ name: String) { self.name = name }
  deinit { print("deinit \(name)") }
}

class Base {
  var number: Int? { _read { yield nil } }
  var text: String? { _read { yield nil } }
  subscript(i: Int) -> Int? { _read { yield nil } }
  var closure: (borrowing Tracked) -> () { _read { yield { _ in } } }
}

class Derived: Base {
  override var number: Int { _read { yield 42 } }
  override var text: String { _read { yield "hello" } }
  override subscript(i: Int) -> Int { _read { yield i * 2 } }
  override var closure: (consuming Tracked) -> () {
    _read { yield { print("consumed \($0.name)") } }
  }
}

class GenericBase<T> {
  var value: T? { _read { yield nil } }
}

class GenericDerived<U>: GenericBase<U> {
  let stored: U
  init(_ stored: U) { self.stored = stored }
  override var value: U { _read { yield stored } }
}

@inline(never)
func makeBase() -> Base { Derived() }

@inline(never)
func makeGenericBase() -> GenericBase<String> { GenericDerived("generic") }

@inline(never)
func testGeneric(_ b: GenericBase<String>) {
  print(b.value ?? "nil")
}

@inline(never)
func test(_ b: Base) {
  print(b.number?.description ?? "nil")
  print(b.text?.count ?? -1)
  print(b[21]?.description ?? "nil")

  let t = Tracked("arg")
  b.closure(t)
  print("after closure")
  withExtendedLifetime(t) {}
}

// CHECK:      42
// CHECK-NEXT: 5
// CHECK-NEXT: 42
// CHECK-NEXT: consumed arg
// CHECK-NEXT: after closure
// CHECK-NEXT: deinit arg
// CHECK-NEXT: generic
// CHECK-NEXT: done
test(makeBase())
testGeneric(makeGenericBase())
print("done")
