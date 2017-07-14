// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t -I %S/Inputs/xref-nested-clang-type/ %s -module-name Lib

// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/xref-nested-clang-type/ %s -DCLIENT -verify

#if CLIENT

import Lib

func test(x: MyInner) {}

#else

import NestedClangTypes

public typealias MyOuter = Outer
public typealias MyInner = Outer.InterestingValue

extension MyOuter {
  public func use(inner: MyInner) {}
}

#endif