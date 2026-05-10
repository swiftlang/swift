// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-builtin-module -enable-experimental-feature Lifetimes)

// REQUIRES: executable_test
// REQUIRES: synchronization
// REQUIRES: swift_feature_Lifetimes

import Builtin
import Synchronization

import StdlibUnittest

struct Foo: ~Copyable {
  var x = 128

  mutating func bar() {
    x &+= 1
  }
}

@available(SwiftStdlib 6.4, *)
struct BorrowingAtomic: ~Escapable {
  let atomic: Ref<Atomic<Int>>

  @_lifetime(borrow a)
  init(_ a: borrowing Atomic<Int>) {
    atomic = Ref(a)
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
  mutating func mutate() -> MutableRef<Wrapped>? {
    if self == nil {
      return nil
    }

    let ptr = UnsafeMutablePointer<Wrapped>(Builtin.unprotectedAddressOf(&self))
    return MutableRef(unsafeAddress: ptr, mutating: &self)
  }

  @available(SwiftStdlib 6.4, *)
  @_lifetime(&self)
  mutating func insert(_ new: consuming Wrapped) -> MutableRef<Wrapped> {
    self = .some(new)
    return mutate()._consumingUnsafelyUnwrap()
  }
}

let suite = TestSuite("RefMutableRef")

if #available(SwiftStdlib 6.4, *) {
suite.test("Ref") {
  var foo = Foo()

  do {
    let borrowFoo = Ref(foo)

    expectEqual(borrowFoo.value.x, 128)
  }

  foo.bar()

  let a = Atomic(123)
  let ba = BorrowingAtomic(a)
  var int = ba.load()

  expectEqual(int, 123)

  ba.store(321)
  int = ba.load()

  expectEqual(int, 321)
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("Ref inside a Ref") {
  var foo = Foo()

  do {
    let borrowFoo = Ref(foo)
    let borrowBorrowFoo = Ref(borrowFoo)

    expectEqual(borrowBorrowFoo.value.value.x, 128)
  }

  foo.bar()

  let a = Atomic(123)
  let ba = BorrowingAtomic(a)
  let borrowBa = Ref(ba)
  var int = borrowBa.value.load()

  expectEqual(int, 123)

  borrowBa.value.store(321)
  int = borrowBa.value.load()

  expectEqual(int, 321)
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("MutableRef") {
  var x: _? = 123

  var y = x.mutate()!
  y.value &+= 321

  expectEqual(y.value, 444)

  x? &-= 321

  expectEqual(x, 123)

  var a: Int? = nil

  var b = a.insert(128)

  expectEqual(b.value, 128)

  b.value &-= 64

  expectEqual(b.value, 64)

  expectEqual(a, 64)
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("MutableRef inside a Ref") {
  var x: _? = 123

  var y = x.mutate()!

  do {
    let borrowY = Ref(y)

    // The following correctly doesn't compile. We cannot mutate a value within
    // a `MutableRef` if it is behind a `Ref`.
    // borrowY.value.value &+= 321

    // But we should be able to read its value though.
    expectEqual(borrowY.value.value, 123)
  }

  var a: Int? = nil

  var b = a.insert(128)

  do {
    let borrowB = Ref(b)

    expectEqual(borrowB.value.value, 128)

    // Again, the following shouldn't compile (it doesn't).
    // borrowB.value.value &-= 64
  }

  expectEqual(a, 128)
}
}

runAllTests()
