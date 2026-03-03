// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-builtin-module -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowInout) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: synchronization
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowInout

import Builtin
import Synchronization

struct Foo: ~Copyable {
  var x = 128

  mutating func bar() {
    x &+= 1
  }
}

@available(SwiftStdlib 6.4, *)
struct BorrowingAtomic: ~Escapable {
  let atomic: Borrow<Atomic<Int>>

  @_lifetime(borrow a)
  init(_ a: borrowing Atomic<Int>) {
    atomic = Borrow(a)
  }

  func load() -> Int {
    atomic.value.load(ordering: .relaxed)
  }

  func store(_ new: Int) {
    atomic.value.store(new, ordering: .relaxed)
  }
}

extension Optional where Wrapped: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  mutating func mutate() -> Inout<Wrapped>? {
    if self == nil {
      return nil
    }

    let ptr = UnsafeMutablePointer<Wrapped>(Builtin.unprotectedAddressOf(&self))
    return Inout(unsafeAddress: ptr, mutating: &self)
  }

  @available(SwiftStdlib 6.4, *)
  @_lifetime(&self)
  mutating func insert(_ new: consuming Wrapped) -> Inout<Wrapped> {
    self = .some(new)
    return mutate()._consumingUnsafelyUnwrap()
  }
}

@available(SwiftStdlib 6.4, *)
func testBorrow() {
  var foo = Foo()

  do {
    let borrowFoo = Borrow(foo)

    // CHECK: 128
    print(borrowFoo.value.x)
  }

  foo.bar()

  let a = Atomic(123)
  let ba = BorrowingAtomic(a)
  var int = ba.load()

  // CHECK: 123
  print(int)

  ba.store(321)
  int = ba.load()

  // CHECK: 321
  print(int)
}

@available(SwiftStdlib 6.4, *)
func testInout() {
  var x: _? = 123

  var y = x.mutate()!
  y.value &+= 321

  // CHECK: 444
  print(y.value)

  x? &-= 321

  // CHECK: 123
  print(x)

  var a: Int? = nil

  var b = a.insert(128)

  // CHECK: 128
  print(b.value)

  b.value &-= 64

  // CHECK: 64
  print(b.value)

  // CHECK: Optional(64)
  print(a)
}

if #available(SwiftStdlib 6.4, *) {
  testBorrow()
  testInout()
}

