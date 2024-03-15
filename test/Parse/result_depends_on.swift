// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonescapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

class MethodModifiers {
    _resultDependsOnSelf func getDependentResult() -> Builtin.NativeObject {
      return Builtin.unsafeCastToNativeObject(self)
    }
}

func testTypeSpecifier(x : _resultDependsOn Klass) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(x)
}

func testMultipleTypeSpecifier(x : _resultDependsOn Klass, y : _resultDependsOn Klass) -> (Builtin.NativeObject, Builtin.NativeObject) {
  return (Builtin.unsafeCastToNativeObject(x), Builtin.unsafeCastToNativeObject(x))
}

// rdar://118125715 (Combining parameter modifiers doesn't work in the new swift parser)
func testTypeSpecifierBorrowing(x : borrowing _resultDependsOn Klass) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(copy x)
}
