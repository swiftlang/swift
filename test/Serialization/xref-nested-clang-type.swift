// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t -I %S/Inputs/xref-nested-clang-type/ %s -module-name Lib
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %t -I %S/Inputs/xref-nested-clang-type/ %s -DCLIENT -verify

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/xref-nested-clang-type/NestedClangTypes.h %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %t/NestedClangTypes.h -I %S/Inputs/xref-nested-clang-type/ %s -module-name Lib -DNO_MODULE
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %t -I %S/Inputs/xref-nested-clang-type/ -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %t/NestedClangTypes.h %s -DCLIENT -verify

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/xref-nested-clang-type/NestedClangTypes.h %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %t/NestedClangTypes.h -I %S/Inputs/xref-nested-clang-type/ -pch-output-dir %t/PCHCache %s -module-name Lib -DNO_MODULE
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %t -I %S/Inputs/xref-nested-clang-type/ -pch-output-dir %t/PCHCache -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %t/NestedClangTypes.h %s -DCLIENT -verify

#if _runtime(_ObjC)
import Foundation
#endif // ObjC

#if CLIENT

import Lib

func test(x: MyInner) {}
func test(x: MyOtherInner) {}
func test(x: MyCode) {}

#else // CLIENT

#if !NO_MODULE
import NestedClangTypes
#endif

public typealias MyOuter = Outer
public typealias MyInner = Outer.InterestingValue

extension MyOuter {
  public func use(inner: MyInner) {}
}

public typealias MyOtherInner = OuterFromOtherModule.InterestingValue
extension OuterFromOtherModule {
  public func use(inner: MyOtherInner) {}
}

#if _runtime(_ObjC)
public typealias MyCode = ErrorCodeEnum.Code
extension ErrorCodeEnum {
  public func whatever(code: MyCode) {}
}
#else
public typealias MyCode = Int
#endif // ObjC

#endif // CLIENT
