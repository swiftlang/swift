// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

import gizmo

// CHECK: [[METHOD_TYPE:@.*]] = private unnamed_addr constant [4 x i8] c"@@:\00"
// CHECK: [[CATEGORY_NAME:@.*]] = private unnamed_addr constant [16 x i8] c"objc_extensions\00"

// CHECK: @"_CATEGORY_Gizmo_$_objc_extensions" = private constant
// CHECK:   i8* getelementptr inbounds ([16 x i8]* [[CATEGORY_NAME]], i64 0, i64 0),
// CHECK:   %swift.type* @"OBJC_CLASS_$_Gizmo",
// CHECK:   @"_CATEGORY_INSTANCE_METHODS_Gizmo_$_objc_extensions",
// CHECK:   @"_CATEGORY_CLASS_METHODS_Gizmo_$_objc_extensions",
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8

extension Gizmo {
  func brandNewInstanceMethod() {
  }

  static func brandNewClassMethod() {
  }

  // Overrides an instance method of NSObject
  func init() -> NSObject {
  }

  // Overrides a class method of NSObject
  static func load() {
  }

  // Shadows an original instance method of Gizmo
  func frob() {
  }

  // Shadows an original class method of Gizmo
  static func runce() {
  }
}

/*
 * Make sure that two extensions of the same ObjC class in the same module can
 * coexist by having different category names.
 */

// CHECK: [[CATEGORY_NAME_1:@.*]] = private unnamed_addr constant [17 x i8] c"objc_extensions1\00"

// CHECK: @"_CATEGORY_Gizmo_$_objc_extensions1" = private constant
// CHECK:   i8* getelementptr inbounds ([17 x i8]* [[CATEGORY_NAME_1]], i64 0, i64 0),
// CHECK:   %swift.type* @"OBJC_CLASS_$_Gizmo",
// CHECK:   {{.*}} @"_CATEGORY_INSTANCE_METHODS_Gizmo_$_objc_extensions1",
// CHECK:   {{.*}} @"_CATEGORY_CLASS_METHODS_Gizmo_$_objc_extensions1",
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8

extension Gizmo {
  func brandSpankingNewInstanceMethod() {
  }

  static func brandSpankingNewClassMethod() {
  }
}

/*
 * Check that extensions of Swift subclasses of ObjC objects get categories.
 */

class Hoozit : NSObject {
}

// CHECK: @"_CATEGORY_INSTANCE_METHODS_Hoozit_$_objc_extensions" = private constant
// CHECK:   i32 24,
// CHECK:   i32 1,
// CHECK:   [1 x { i8*, i8*, i8* }] [{ i8*, i8*, i8* } {
// CHECK:     i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(blibble)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void (%CSo6Hoozit*, i8*)* @_TToCSo6Hoozit7blibblefS_FT_T_ to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @"_CATEGORY_CLASS_METHODS_Hoozit_$_objc_extensions" = private constant
// CHECK:   i32 24,
// CHECK:   i32 1,
// CHECK:   [1 x { i8*, i8*, i8* }] [{ i8*, i8*, i8* } {
// CHECK:     i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(blobble)", i64 0, i64 0),
// CHECK:     i8* null,
// CHECK:     i8* bitcast (void (%swift.type*, i8*)* @_TToCSo6Hoozit7blobblefMS_FT_T_ to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @"_CATEGORY_Hoozit_$_objc_extensions" = private constant
// CHECK:   i8* getelementptr inbounds ([16 x i8]* [[CATEGORY_NAME]], i64 0, i64 0),
// CHECK:   %swift.type* getelementptr inbounds ({{.*}} @_TMdCSo6Hoozit {{.*}}),
// CHECK:   {{.*}} @"_CATEGORY_INSTANCE_METHODS_Hoozit_$_objc_extensions",
// CHECK:   {{.*}} @"_CATEGORY_CLASS_METHODS_Hoozit_$_objc_extensions",
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8

extension Hoozit {
  func blibble() { }
  static func blobble() { }
}

/*
 * Check that extension methods get called as ObjC methods.
 */

// CHECK: define void @_T15objc_extensions13messWithGizmoFT1gCSo5Gizmo_T_(%CSo5Gizmo* %g) {
func messWithGizmo(g:Gizmo) {
  // CHECK: load i8** @"\01L_selector(brandNewInstanceMethod)"
  g.brandNewInstanceMethod()
  // CHECK: load i8** @"\01L_selector(brandSpankingNewInstanceMethod)"
  g.brandSpankingNewInstanceMethod()
  // FIXME: typechecks as ambiguous
  // C/HECK: load i8** @"\01L_selector(init)"
  //g.init()
  // CHECK: load i8** @"\01L_selector(frob)"
  g.frob()

  // CHECK: load i8** @"\01L_selector(brandNewClassMethod)"
  Gizmo.brandNewClassMethod()
  // CHECK: load i8** @"\01L_selector(brandSpankingNewClassMethod)"
  Gizmo.brandSpankingNewClassMethod()
  // FIXME: typechecks as ambiguous
  // C/HECK: load i8** @"\01L_selector(load)"
  //Gizmo.load()
  // CHECK: load i8** @"\01L_selector(runce)"
  Gizmo.runce()
}

// CHECK: define void @_T15objc_extensions14messWithHoozitFT1hCSo6Hoozit_T_(%CSo6Hoozit* %h) {
func messWithHoozit(h:Hoozit) {
  // CHECK: load i8** @"\01L_selector(blibble)"
  h.blibble()
  // CHECK: load i8** @"\01L_selector(blobble)"
  Hoozit.blobble()
}
