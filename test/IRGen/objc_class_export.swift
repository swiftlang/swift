// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -sdk %S/Inputs -I %t -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: [[HOOZIT:%C17objc_class_export6Hoozit]] = type <{ [[REF:%swift.refcounted]] }>
// CHECK: [[REF]] = type
// CHECK: [[FOO:%C17objc_class_export3Foo]] = type <{ [[REF]], %Si }>
// CHECK: [[INT:%Si]] = type <{ i64 }>
// CHECK: [[NSRECT:%VSC6NSRect]] = type <{ %VSC7NSPoint, %VSC6NSSize }>
// CHECK: [[NSPOINT:%VSC7NSPoint]] = type <{ %Sd, %Sd }>
// CHECK: [[DOUBLE:%Sd]] = type <{ double }>
// CHECK: [[NSSIZE:%VSC6NSSize]] = type <{ %Sd, %Sd }>
// CHECK: [[OBJC:%objc_object]] = type opaque

// CHECK: @"OBJC_METACLASS_$__TtC17objc_class_export3Foo" = global %objc_class {
// CHECK:   %objc_class* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   %objc_class* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 ptrtoint ({{.*}}* @_METACLASS_DATA__TtC17objc_class_export3Foo to i64)
// CHECK: }
// CHECK: [[FOO_NAME:@.*]] = private unnamed_addr constant [28 x i8] c"_TtC17objc_class_export3Foo\00"
// CHECK: @_METACLASS_DATA__TtC17objc_class_export3Foo = private constant {{.*\*}} } {
// CHECK:   i32 129,
// CHECK:   i32 40,
// CHECK:   i32 40,
// CHECK:   i32 0,
// CHECK:   i8* null,
// CHECK:   i8* getelementptr inbounds ([{{[0-9]+}} x i8], [{{[0-9]+}} x i8]* [[FOO_NAME]], i64 0, i64 0),
// CHECK:   @_CLASS_METHODS__TtC17objc_class_export3Foo,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_DATA__TtC17objc_class_export3Foo = private constant {{.*\*}} } {
// CHECK:   i32 128,
// CHECK:   i32 16,
// CHECK:   i32 24,
// CHECK:   i32 0,
// CHECK:   i8* null,
// CHECK:   i8* getelementptr inbounds ([{{[0-9]+}} x i8], [{{[0-9]+}} x i8]* [[FOO_NAME]], i64 0, i64 0),
// CHECK:   { i32, i32, [6 x { i8*, i8*, i8* }] }* @_INSTANCE_METHODS__TtC17objc_class_export3Foo,
// CHECK:   i8* null,
// CHECK:   @_IVARS__TtC17objc_class_export3Foo,
// CHECK:   i8* null,
// CHECK:   _PROPERTIES__TtC17objc_class_export3Foo
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_TMdC17objc_class_export3Foo = global {{.*i64}} } {
// CHECK:   void ([[FOO]]*)* @_TFC17objc_class_export3FooD,
// CHECK:   i8** @_TWVBO,
// CHECK:   i64 ptrtoint (%objc_class* @"OBJC_METACLASS_$__TtC17objc_class_export3Foo" to i64),
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 add (i64 ptrtoint ({{.*}}* @_DATA__TtC17objc_class_export3Foo to i64), i64 1),
// CHECK:   [[FOO]]* (%swift.type*)* @_TZFC17objc_class_export3Foo6createfMS0_FT_S0_,
// CHECK:   void (%VSC6NSRect*, [[FOO]]*)* @_TFC17objc_class_export3Foo10drawInRectfS0_FT5dirtyVSC6NSRect_T_
// CHECK: }, section "__DATA,__objc_data, regular, no_dead_strip"
// -- TODO: The OBJC_CLASS symbol should reflect the qualified runtime name of
//    Foo.
// CHECK: @"OBJC_CLASS_$__TtC17objc_class_export3Foo" = alias getelementptr inbounds ({{.*}} @_TMdC17objc_class_export3Foo, i32 0, i32 2)

import gizmo

class Hoozit {}

struct BigStructWithNativeObjects {
  var x, y, w : Double
  var h : Hoozit
}

@objc class Foo {
  var x = 0
  class func create() -> Foo {
    return Foo()
  }

  func drawInRect(dirty dirty: NSRect) {
  }
  // CHECK: define internal void @_TToFC17objc_class_export3Foo10drawInRectfS0_FT5dirtyVSC6NSRect_T_([[OPAQUE:%.*]]*, i8*, [[NSRECT]]* byval align 8) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE]]* %0 to [[FOO]]*
  // CHECK:   call void @_TFC17objc_class_export3Foo10drawInRectfS0_FT5dirtyVSC6NSRect_T_(%VSC6NSRect* byval align {{4|8}} {{.*}}, [[FOO]]* [[CAST]])
  // CHECK: }

  func bounds() -> NSRect {
    return NSRect(origin: NSPoint(x: 0, y: 0), 
                  size: NSSize(width: 0, height: 0))
  }
  // CHECK: define internal void @_TToFC17objc_class_export3Foo6boundsfS0_FT_VSC6NSRect([[NSRECT]]* noalias sret, [[OPAQUE4:%.*]]*, i8*) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE4]]* %1 to [[FOO]]*
  // CHECK:   call void @_TFC17objc_class_export3Foo6boundsfS0_FT_VSC6NSRect([[NSRECT]]* noalias sret {{.*}}, [[FOO]]* [[CAST]])

  func convertRectToBacking(r r: NSRect) -> NSRect {
    return r;
  }
  // CHECK: define internal void @_TToFC17objc_class_export3Foo20convertRectToBackingfS0_FT1rVSC6NSRect_S1_([[NSRECT]]* noalias sret, [[OPAQUE5:%.*]]*, i8*, [[NSRECT]]* byval align 8) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE5]]* %1 to [[FOO]]*
  // CHECK:   call void @_TFC17objc_class_export3Foo20convertRectToBackingfS0_FT1rVSC6NSRect_S1_([[NSRECT]]* noalias sret {{.*}}, %VSC6NSRect* byval align {{4|8}} {{.*}}, [[FOO]]* [[CAST]])

  func doStuffToSwiftSlice(f f: [Int]) {
  }
  // This function is not representable in Objective-C, so don't emit the objc entry point.
  // CHECK-NOT: @_TToFC17objc_class_export3Foo19doStuffToSwiftSlicefS_FT1fGSaSi__T_

  func doStuffToBigSwiftStruct(f f: BigStructWithNativeObjects) {
  }
  // This function is not representable in Objective-C, so don't emit the objc entry point.
  // CHECK-NOT: @_TToFC17objc_class_export3Foo23doStuffToBigSwiftStructfS_FT1fV17objc_class_export27BigStructWithNativeObjects_T_

  init() { }
}

