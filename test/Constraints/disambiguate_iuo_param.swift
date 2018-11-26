// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -import-objc-header %S/Inputs/disambiguate_iuo_param.h %s -emit-ir | %FileCheck %s
// -module-name objc_ir -I %S/Inputs/custom-modules -emit-ir -g -o - -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: define {{.*}} @main
// CHECK: load {{.*}}, {{.*}}@"\01L_selector(isEqualToObject:)"
let c = Obj()
_ = c.isEqual(to: c)
