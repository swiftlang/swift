// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature  NonEscapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}
class MethodModifiers {
    _resultDependsOnSelf func testAttrOnMethod() { } // expected-error{{Incorrect use of _resultDependsOnSelf with no result}}
}

_resultDependsOnSelf func testAttrOnFunc () { } // expected-error{{'resultDependsOnSelf' is only valid on methods}}
