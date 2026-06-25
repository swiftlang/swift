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

suite.test("Ref")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

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

suite.test("MutableRef")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

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

suite.test("Sendability")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  func isSendable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
  func isSendable<T: Sendable & ~Copyable & ~Escapable>(_: T.Type) -> Bool { true }

  expectTrue(isSendable(Ref<Int>.self))
  expectTrue(isSendable(MutableRef<Int>.self))

  expectFalse(isSendable(Ref<UnsafeMutableRawPointer>.self))
  expectFalse(isSendable(MutableRef<UnsafeRawPointer>.self))
}

suite.test("BitwiseCopyability")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  func isBitwiseCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
  func isBitwiseCopyable<T: BitwiseCopyable & ~Escapable>(_: T.Type) -> Bool { true }

  expectTrue(isBitwiseCopyable(Ref<Int>.self))
  expectFalse(isBitwiseCopyable(MutableRef<Int>.self))
}

runAllTests()
