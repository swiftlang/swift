// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonEscapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

class MethodModifiers {
    _resultDependsOnSelf func getDependentResult() -> Builtin.NativeObject {
      return Builtin.unsafeCastToNativeObject(self)
    }
}
