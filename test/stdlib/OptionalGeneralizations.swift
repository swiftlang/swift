// RUN: %target-run-simple-swift(-enable-experimental-feature NonescapableTypes)
// REQUIRES: executable_test
// REQUIRES: reflection
// REQUIRES: swift_feature_NonescapableTypes

import StdlibUnittest
import Swift

let suite = TestSuite("Optional")

defer { runAllTests() }

func isCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
func isCopyable<T: ~Escapable>(_: T.Type) -> Bool { true }

func isBitwiseCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool { false }
func isBitwiseCopyable<T: BitwiseCopyable & ~Escapable>(_: T.Type) -> Bool { true }

#if $NonescapableTypes
func isEscapable<T: ~Escapable & ~Copyable>(_: T.Type) -> Bool { false }
func isEscapable<T: ~Copyable>(_: T.Type) -> Bool { true }
#endif

struct TrivialStruct {}
struct NoncopyableStruct: ~Copyable {}
class RegularClass {}

#if $NonescapableTypes
struct NonescapableStruct: ~Escapable, BitwiseCopyable {}
struct NoncopyableNonescapableStruct: ~Copyable, ~Escapable {}
struct NonescapableNontrivialStruct: ~Escapable {
  let foo: RegularClass? = nil
}
#endif

suite.test("Copyability") {
  expectTrue(isCopyable(Optional<TrivialStruct>.self))
  expectFalse(isCopyable(Optional<NoncopyableStruct>.self))
  expectTrue(isCopyable(Optional<RegularClass>.self))
#if $NonescapableTypes
  expectTrue(isCopyable(Optional<NonescapableStruct>.self))
  expectFalse(isCopyable(Optional<NoncopyableNonescapableStruct>.self))
  expectTrue(isCopyable(Optional<NonescapableNontrivialStruct>.self))
#endif
}

suite.test("BitwiseCopyability") {
  expectTrue(isBitwiseCopyable(Optional<TrivialStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NoncopyableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<RegularClass>.self))
#if $NonescapableTypes
  expectTrue(isBitwiseCopyable(Optional<NonescapableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NoncopyableNonescapableStruct>.self))
  expectFalse(isBitwiseCopyable(Optional<NonescapableNontrivialStruct>.self))
#endif
}

#if $NonescapableTypes
suite.test("Escapability") {
  expectTrue(isEscapable(Optional<TrivialStruct>.self))
  expectTrue(isEscapable(Optional<NoncopyableStruct>.self))
  expectTrue(isEscapable(Optional<RegularClass>.self))
  expectFalse(isEscapable(Optional<NonescapableStruct>.self))
  expectFalse(isEscapable(Optional<NoncopyableNonescapableStruct>.self))
  expectFalse(isEscapable(Optional<NonescapableNontrivialStruct>.self))
}
#endif
