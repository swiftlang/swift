// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-enable-builtin-module -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowInout)

// REQUIRES: executable_test
// REQUIRES: synchronization
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowInout

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

let suite = TestSuite("BorrowInout")

if #available(SwiftStdlib 6.4, *) {
suite.test("Borrow") {
  var foo = Foo()

  do {
    let borrowFoo = Borrow(foo)

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
suite.test("Inout") {
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

runAllTests()
