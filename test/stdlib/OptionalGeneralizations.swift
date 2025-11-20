// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes)
// REQUIRES: executable_test
// REQUIRES: reflection
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest
import Swift

let suite = TestSuite("Optional")

defer { runAllTests() }

func isCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
func isCopyable<T: ~Escapable>(_: T.Type) -> Bool { true }

func isBitwiseCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
func isBitwiseCopyable<T: BitwiseCopyable & ~Escapable>(_: T.Type) -> Bool { true }

func isEscapable<T: ~Escapable & ~Copyable>(_: T.Type) -> Bool { false }
func isEscapable<T: ~Copyable>(_: T.Type) -> Bool { true }

struct TrivialStruct {}
struct NoncopyableStruct: ~Copyable {}
class RegularClass {}

struct NonescapableStruct: ~Escapable, BitwiseCopyable {}
struct NoncopyableNonescapableStruct: ~Copyable, ~Escapable {}
struct NonescapableNontrivialStruct: ~Escapable {
  let foo: RegularClass? = nil
}

suite.test("Copyability") {
  expectTrue(isCopyable(Optional<TrivialStruct>.self))
  expectFalse(isCopyable(Optional<NoncopyableStruct>.self))
  expectTrue(isCopyable(Optional<RegularClass>.self))
  expectTrue(isCopyable(Optional<NonescapableStruct>.self))
  expectFalse(isCopyable(Optional<NoncopyableNonescapableStruct>.self))
  expectTrue(isCopyable(Optional<NonescapableNontrivialStruct>.self))
}

suite.test("BitwiseCopyability") {
  expectTrue(isBitwiseCopyable(Optional<TrivialStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NoncopyableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<RegularClass>.self))
  expectTrue(isBitwiseCopyable(Optional<NonescapableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NoncopyableNonescapableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NonescapableNontrivialStruct>.self))
}

suite.test("Escapability") {
  expectTrue(isEscapable(Optional<TrivialStruct>.self))
  expectTrue(isEscapable(Optional<NoncopyableStruct>.self))
  expectTrue(isEscapable(Optional<RegularClass>.self))
  expectFalse(isEscapable(Optional<NonescapableStruct>.self))
  expectFalse(isEscapable(Optional<NoncopyableNonescapableStruct>.self))
  expectFalse(isEscapable(Optional<NonescapableNontrivialStruct>.self))
}

func apply<T, U>(
  _ input: T,
  _ body: (T) -> U
) -> U {
  body(input)
}

func apply2<T: ~Copyable, U: ~Copyable>(
  _ input: consuming T,
  _ body: (consuming T) -> U
) -> U {
  body(input)
}

suite.test("Initializer references") {
  do {
    let r = apply(TrivialStruct(), Optional.init)
    expectTrue(r != nil)
  }

  do {
    let r = apply2(NoncopyableStruct(), Optional.init)
    expectTrue(r != nil)
  }
}

suite.test("expectNotNil()") {
  func opt1<T: ~Copyable>(_ t: consuming T) -> T? { Optional.some(t) }
  _ = expectNotNil(opt1(TrivialStruct()))
  _ = expectNotNil(opt1(NoncopyableStruct()))
  _ = expectNotNil(opt1(RegularClass()))
  @_lifetime(copy t)
  func opt2<T: ~Copyable & ~Escapable>(_ t: consuming T) -> T? { t }

  let ne = NonescapableStruct()
  _ = expectNotNil(opt2(ne))

  let ncne = NoncopyableNonescapableStruct()
  _ = expectNotNil(opt2(ncne))

  let nent = NonescapableNontrivialStruct()
  _ = expectNotNil(opt2(nent))
}

suite.test("expectNil()") {
  func opt1<T: ~Copyable>(_ t: consuming T) -> T? { nil }
  expectNil(opt1(TrivialStruct()))
  expectNil(opt1(NoncopyableStruct()))
  expectNil(opt1(RegularClass()))
  @_lifetime(copy t)
  func opt2<T: ~Copyable & ~Escapable>(_ t: consuming T) -> T? { nil }

  let ne = NonescapableStruct()
  expectNil(opt2(ne))

  let ncne = NoncopyableNonescapableStruct()
  expectNil(opt2(ncne))

  let nent = NonescapableNontrivialStruct()
  expectNil(opt2(nent))
}
