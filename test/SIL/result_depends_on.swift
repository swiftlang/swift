// RUN: %target-swift-frontend %s -emit-sil  -enable-builtin-module -enable-experimental-feature NonescapableTypes -disable-experimental-parser-round-trip
// REQUIRES: asserts

import Builtin

class Klass {}

class MethodModifiers {
// CHECK-LABEL: sil hidden [ossa] [_resultDependsOnSelf] @$s17result_depends_on15MethodModifiersC18getDependentResultBoyF :
    _resultDependsOnSelf func getDependentResult() -> Builtin.NativeObject {
      return Builtin.unsafeCastToNativeObject(self)
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s17result_depends_on3fooyBoAA5KlassCF :
// CHECK: bb0(%0 : @_resultDependsOn @guaranteed $Klass):
// CHECK: } // end sil function '$s17result_depends_on3fooyBoAA5KlassCF'
func foo(_ x : _resultDependsOn Klass) -> Builtin.NativeObject {
  return Builtin.unsafeCastToNativeObject(x)
}

