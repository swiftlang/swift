// RUN: %target-typecheck-verify-swift  -enable-builtin-module -enable-experimental-feature NonEscapableTypes
// REQUIRES: asserts

import Builtin

class Klass {}

class borrowing {}

class _resultDependsOnSelf {}

class MethodModifiers {
    func getNoOpReturn() -> _resultDependsOnSelf {
      return _resultDependsOnSelf()
    }
}
