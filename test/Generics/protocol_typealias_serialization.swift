// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/protocol_typealias_other.swift -emit-module-path %t/protocol_typealias_other.swiftmodule -module-name protocol_typealias_other
// RUN: %target-swift-frontend -typecheck %s -I %t -debug-generic-signatures 2>&1 | %FileCheck %s

import protocol_typealias_other

// CHECK-LABEL: .useP1@
// CHECK-NEXT: <T where T : P1, T.[P1]Y.[P2]Z == Int>
func useP1<T : P1>(_: T) where T.X == Int {}

// CHECK-LABEL: .useP2@
// CHECK-NEXT: <T where T : P2, T.[P2]Z == Int>
func useP2<T : P2>(_: T) where T.C == Array<Int> {}
