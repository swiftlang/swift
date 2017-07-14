// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t -I %S/Inputs/xref-nested-clang-type/ %s -module-name Lib

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %t -I %S/Inputs/xref-nested-clang-type/ %s -DCLIENT -verify

#if CLIENT

import Lib

func test(x: MyInner) {}

#if _runtime(_ObjC)
import Foundation
func test(x: MyCode) {}
#endif // ObjC

#else // CLIENT

import NestedClangTypes

public typealias MyOuter = Outer
public typealias MyInner = Outer.InterestingValue

extension MyOuter {
  public func use(inner: MyInner) {}
}

#if _runtime(_ObjC)
import Foundation

public typealias MyCode = ErrorCodeEnum.Code
extension ErrorCodeEnum {
  public func whatever(code: MyCode) {}
}
#endif // ObjC

#endif // CLIENT
