// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature  NonescapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

struct View {
  var pointer: UnsafeRawPointer?
}

class MethodModifiers {
    _resultDependsOnSelf func testAttrOnMethod() { } // expected-error{{Incorrect use of _resultDependsOnSelf with no result}}
}

_resultDependsOnSelf func testAttrOnFunc () -> View { return View(pointer:nil) } // expected-error{{only methods can be declared '_resultDependsOnSelf'}}

// TODO: Seems like Sema doesn't have enough info to diagnose this
func testTypeSpecifierNoResult(x : _resultDependsOn Klass) { } // todo{{'_resultDependsOn' is only valid when there is a result}}

// TODO: Sema doesn't know if a type is non-trivial, diagnose this in SILGen maybe ?
func testTypeSpecifierTrivial(x : _resultDependsOn Int) -> Int { 0 } // todo{{'_resultDependsOnSelf' is only valid on non-trivial types}}
