// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs -enable-cxx-interop -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import ConstructorsObjC

// CHECK: [[VAR:%[0-9]+]] = alloc_stack $ConstructorWithNSArrayParam
// CHECK: [[TYPE:%[0-9]+]] = metatype $@thin ConstructorWithNSArrayParam.Type
// CHECK: [[OPT_ARRAY:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt, %{{[0-9]+}} : $NSArray
// CHECK: [[FUNC:%[0-9]+]] = function_ref @_ZN27ConstructorWithNSArrayParamC1EP7NSArray : $@convention(c) (Optional<NSArray>, @thin ConstructorWithNSArrayParam.Type) -> @out ConstructorWithNSArrayParam
// CHECK: %{{[0-9]+}} = apply [[FUNC]]([[VAR]], [[OPT_ARRAY]], [[TYPE]]) : $@convention(c) (Optional<NSArray>, @thin ConstructorWithNSArrayParam.Type) -> @out ConstructorWithNSArrayParam
let _ = ConstructorWithNSArrayParam([])
