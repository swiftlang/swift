// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name UseObjCFrozenEnum -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -I %S/Inputs/ -typecheck -verify -emit-clang-header-path %t/UseObjCFrozenEnum.h

// RUN: echo "#include \"ObjCFrozenEnum.h\"" > %t/full-header.h
// RUN: cat %t/UseObjCFrozenEnum.h >> %t/full-header.h

// RUN: %check-interop-cxx-header-in-clang(%t/full-header.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -I %S/Inputs)
// RUN: %FileCheck %s < %t/full-header.h

import ObjCFrozenEnum

public func returnsObjCFrozenEnumOpt() -> ObjCFrozenEnum? {
    return nil
}

public func takesObjCFrozenEnumOpt(_ x: ObjCFrozenEnum?) {}

// CHECK: isUsableInGenericContext<ObjCFrozenEnum>
// CHECK: TypeMetadataTrait<ObjCFrozenEnum>
