// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/disambiguate_iuo_param.h %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: define {{.*}} @main
// CHECK: load {{.*}}, {{.*}}@"\01L_selector(isEqualToObject:)"
let c = Obj()
_ = c.isEqual(to: c)
