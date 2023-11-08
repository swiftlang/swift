// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonEscapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

var _resultDependsOn = 0
var _resultDependsOnSelf = 100.9

class MethodModifiers {
    _resultDependsOnSelf func _resultDependsOnSelf() -> Builtin.NativeObject {
      return Builtin.unsafeCastToNativeObject(self)
    }
}

