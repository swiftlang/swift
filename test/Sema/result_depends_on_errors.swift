// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature  NonEscapableTypes
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
