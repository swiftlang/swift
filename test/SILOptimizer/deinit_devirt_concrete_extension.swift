// RUN: %target-swift-frontend -primary-file %s -parse-as-library -O -module-name=test -emit-sil -o /dev/null

// REQUIRES: swift_in_compiler

// Regression test for a DeinitDevirtualizer assertion failure with ~Copyable
// types nested in constrained extensions where all generic parameters are
// concrete (https://github.com/swiftlang/swift/issues/88213).
//
// The deinit's SILFunctionType has no invocation generic signature when all
// parameters are fixed, but getContextSubstitutionMap returns a non-empty map
// for the enclosing generic context. The DeinitDevirtualizer must check
// isGeneric before passing substitutions to createApply.
//
// Requires -primary-file mode; WMO does not trigger the DeinitDevirtualizer
// on these functions. Uses a Swift source test because the SIL parser's
// sil_moveonlydeinit directive cannot resolve extension-nested types.

enum Container<Element> {}

extension Container where Element == UInt8 {
  struct Storage: ~Copyable {
    var value: Int
    deinit {}
  }

  struct Wrapper: ~Copyable {
    var _storage: Storage
  }
}

// Exercises DestroyValueInst.createDeinitCall: consuming parameter is
// destroyed at end of function.
func consumeValue(_ s: consuming Container<UInt8>.Storage) {}

// Exercises DestroyAddrInst.createDeinitCall: assignment to stored property
// destroys old value in place.
func replaceStorage(_ w: inout Container<UInt8>.Wrapper) {
  w._storage = .init(value: 42)
}
