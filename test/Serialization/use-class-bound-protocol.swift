// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module-path %t/ClassBoundProtocol.swiftmodule %S/Inputs/class-bound-protocol.swift -module-name ClassBoundProtocol
// RUN: %target-swift-frontend -typecheck %s -I %t

import ClassBoundProtocol

func f() {
  let p1: any P1 = D()
  p1.funcInClass()
  p1.funcInBaseProtocol()

  let p2: any P2 = D()
  p2.funcInClass()
  p2.funcInBaseProtocol()

  let genericP1: any GenericP1 = GenericD<Int>()
  genericP1.funcInClass()
  genericP1.funcInBaseProtocol()

  let genericP2: any GenericP2 = GenericD<Int>()
  genericP2.funcInClass()
  genericP2.funcInBaseProtocol()
}
