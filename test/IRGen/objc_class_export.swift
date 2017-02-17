// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -sdk %S/Inputs -I %t -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: %swift.refcounted = type
// CHECK: [[HOOZIT:%C17objc_class_export6Hoozit]] = type <{ [[REF:%swift.refcounted]] }>
// CHECK: [[FOO:%C17objc_class_export3Foo]] = type <{ [[REF]], %Si }>
// CHECK: [[INT:%Si]] = type <{ i64 }>
// CHECK: [[DOUBLE:%Sd]] = type <{ double }>
// CHECK: [[NSRECT:%VSC6NSRect]] = type <{ %VSC7NSPoint, %VSC6NSSize }>
// CHECK: [[NSPOINT:%VSC7NSPoint]] = type <{ %Sd, %Sd }>
// CHECK: [[NSSIZE:%VSC6NSSize]] = type <{ %Sd, %Sd }>
// CHECK: [[OBJC:%objc_object]] = type opaque

// CHECK: @"OBJC_METACLASS_$__TtC17objc_class_export3Foo" = hidden global %objc_class {
// CHECK:   %objc_class* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   %objc_class* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* null,
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
// CHECK: @_TMfC17objc_class_export3Foo = internal global <{{.*i64}} }> <{
// CHECK:   void ([[FOO]]*)* @_TFC17objc_class_export3FooD,
// CHECK:   i8** @_TWVBO,
// CHECK:   i64 ptrtoint (%objc_class* @"OBJC_METACLASS_$__TtC17objc_class_export3Foo" to i64),
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* null,
// CHECK:   i64 add (i64 ptrtoint ({{.*}}* @_DATA__TtC17objc_class_export3Foo to i64), i64 1),
// CHECK:   [[FOO]]* (%swift.type*)* @_TZFC17objc_class_export3Foo6createfT_S0_,
// CHECK:   void (double, double, double, double, [[FOO]]*)* @_TFC17objc_class_export3Foo10drawInRectfT5dirtyVSC6NSRect_T_
// CHECK: }>, section "__DATA,__objc_data, regular"
// -- TODO: The OBJC_CLASS symbol should reflect the qualified runtime name of
//    Foo.
// CHECK: @_TMC17objc_class_export3Foo = hidden alias %swift.type, bitcast (i64* getelementptr inbounds ({{.*}} @_TMfC17objc_class_export3Foo, i32 0, i32 2) to %swift.type*)
// CHECK: @"OBJC_CLASS_$__TtC17objc_class_export3Foo" = alias %swift.type, %swift.type* @_TMC17objc_class_export3Foo

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
  // CHECK: define internal void @_TToFC17objc_class_export3Foo10drawInRectfT5dirtyVSC6NSRect_T_([[OPAQUE:%.*]]*, i8*, [[NSRECT]]* byval align 8) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE]]* %0 to [[FOO]]*
  // CHECK:   call swiftcc void @_TFC17objc_class_export3Foo10drawInRectfT5dirtyVSC6NSRect_T_(double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, [[FOO]]* swiftself [[CAST]])
  // CHECK: }

  func bounds() -> NSRect {
    return NSRect(origin: NSPoint(x: 0, y: 0), 
                  size: NSSize(width: 0, height: 0))
  }
  // CHECK: define internal void @_TToFC17objc_class_export3Foo6boundsfT_VSC6NSRect([[NSRECT]]* noalias nocapture sret, [[OPAQUE4:%.*]]*, i8*) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE4]]* %1 to [[FOO]]*
  // CHECK:   call swiftcc { double, double, double, double } @_TFC17objc_class_export3Foo6boundsfT_VSC6NSRect([[FOO]]* swiftself [[CAST]])

  func convertRectToBacking(r r: NSRect) -> NSRect {
    return r
  }
  // CHECK: define internal void @_TToFC17objc_class_export3Foo20convertRectToBackingfT1rVSC6NSRect_S1_([[NSRECT]]* noalias nocapture sret, [[OPAQUE5:%.*]]*, i8*, [[NSRECT]]* byval align 8) unnamed_addr {{.*}} {
  // CHECK:   [[CAST:%[a-zA-Z0-9]+]] = bitcast [[OPAQUE5]]* %1 to [[FOO]]*
  // CHECK:   call swiftcc { double, double, double, double } @_TFC17objc_class_export3Foo20convertRectToBackingfT1rVSC6NSRect_S1_(double {{.*}}, double {{.*}}, double {{.*}}, double {{.*}}, [[FOO]]* swiftself [[CAST]])

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

