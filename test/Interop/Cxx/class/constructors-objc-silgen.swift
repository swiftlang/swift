// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs -enable-experimental-cxx-interop -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop
import Foundation
import ConstructorsObjC

// CHECK: [[VAR:%[0-9]+]] = alloc_stack $ConstructorWithNSArrayParam
// CHECK: [[OPT_ARRAY:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt, %{{[0-9]+}} : $NSArray
// CHECK: [[FUNC:%[0-9]+]] = function_ref @_ZN27ConstructorWithNSArrayParamC1EP7NSArray : $@convention(c) (Optional<NSArray>) -> @out ConstructorWithNSArrayParam
// CHECK: %{{[0-9]+}} = apply [[FUNC]]([[VAR]], [[OPT_ARRAY]]) : $@convention(c) (Optional<NSArray>) -> @out ConstructorWithNSArrayParam
let _ = ConstructorWithNSArrayParam([])
