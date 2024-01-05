// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonescapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

class borrowing {}

class _resultDependsOn {}
class _resultDependsOnSelf {}

class MethodModifiers {
    func getNoOpReturn() -> _resultDependsOnSelf {
      return _resultDependsOnSelf()
    }
}

func testNoopArgLabel(_resultDependsOn : _resultDependsOn Klass) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(_resultDependsOn)
}

func testNoopParamName(_resultDependsOn x: _resultDependsOn Klass) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(x)
}

func testNoopLocalName(x: _resultDependsOn Klass) -> Builtin.NativeObject {
  let _resultDependsOn = Builtin.unsafeCastToNativeObject(x)
  return _resultDependsOn
}

/*
func testNoopParamNameTypeName(resultDependsOn x: resultDependsOn resultDependsOn) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(x)
}
*/

// Also test function names, global variable names
