// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/sr8945-other.swift -emit-module-path %t/other.swiftmodule -module-name other
// RUN: %target-swift-frontend -emit-silgen %s -I%t

import other

public class C : P {
  public typealias T = Int
}

public func takesInt(_: Int) {}

public func foo<T : C, S : Sequence>(_: T, _ xs: S) where S.Element == T.T {
  for x in xs {
    takesInt(x)
  }
}
