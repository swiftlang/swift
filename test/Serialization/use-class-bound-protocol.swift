// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module-path %t/ClassBoundProtocol.swiftmodule %S/Inputs/class-bound-protocol.swift -module-name ClassBoundProtocol
// RUN: %target-swift-frontend -typecheck %s -I %t

import ClassBoundProtocol

func f() {
  let p: P = C()
  p.funcInClass()

  let genericP: GenericP = GenericC<Int>()
  genericP.funcInClass()
}
