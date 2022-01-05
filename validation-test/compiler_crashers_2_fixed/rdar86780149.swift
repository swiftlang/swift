// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/A.swiftmodule %S/Inputs/rdar86780149/A.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/B.swiftmodule %S/Inputs/rdar86780149/B.swiftinterface -I %t
// RUN: %target-swift-frontend -emit-silgen -I %t %s

import A
import B

struct Foo: Equatable {
  var x: Value.State
}
