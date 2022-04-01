// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/sr8945-other.swift -emit-module-path %t/other.swiftmodule -module-name other
// RUN: %target-swift-frontend -emit-silgen %s -I%t -debug-generic-signatures 2>&1 | %FileCheck %s

import other

public class C : P {
  public typealias T = Int
}

public func takesInt(_: Int) {}

// CHECK-LABEL: .foo@
// CHECK-NEXT: Generic signature: <T, S where T : C, S : Sequence, S.[Sequence]Element == Int>
public func foo<T : C, S : Sequence>(_: T, _ xs: S) where S.Element == T.T {
  for x in xs {
    takesInt(x)
  }
}
