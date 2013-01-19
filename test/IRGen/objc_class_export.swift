// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK: [[OBJC_CLASS:%objc_class]] = type
// CHECK: [[OPAQUE:%swift.opaque]] = type
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[REF:%swift.refcounted]] = type
// CHECK: [[FOO:%C17objc_class_export3Foo]] = type { [[REF]], %Si }
// CHECK: [[INT:%Si]] = type { i64 }
// CHECK: [[FULL_HEAPMETADATA:%swift.full_heapmetadata]] = type
// CHECK: [[OBJC:%objc_object]] = type opaque

// CHECK: @"OBJC_METACLASS_$_Foo" = global [[OBJC_CLASS]] {
// CHECK:   [[OBJC_CLASS]]* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   [[OBJC_CLASS]]* @"OBJC_METACLASS_$_SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* @_objc_empty_vtable,
// CHECK:   i64 ptrtoint ({{.*}}* @_METACLASS_DATA_Foo to i64)
// CHECK: }
// CHECK: [[FOO_NAME:@.*]] = private unnamed_addr constant [4 x i8] c"Foo\00"
// CHECK: @_METACLASS_DATA_Foo = private constant {{.*\*}} } {
// CHECK:   i32 129,
// CHECK:   i32 0,
// CHECK:   i32 0,
// CHECK:   i32 0,
// CHECK:   i8* null,
// CHECK:   i8* getelementptr inbounds ([4 x i8]* [[FOO_NAME]], i64 0, i64 0),
// CHECK:   @_CLASS_METHODS_Foo,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_DATA_Foo = private constant {{.*\*}} } {
// CHECK:   i32 128,
// CHECK:   i32 16,
// CHECK:   i32 24,
// CHECK:   i32 0,
// CHECK:   i8* null,
// CHECK:   i8* getelementptr inbounds ([4 x i8]* [[FOO_NAME]], i64 0, i64 0),
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   @_IVARS_Foo,
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_TMdC17objc_class_export3Foo = global {{.*\*}} } {
// CHECK:   i64 ([[REF]]*)* @_TLC17objc_class_export3FooD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @"OBJC_METACLASS_$_Foo" to i64),
// CHECK:   [[TYPE]]* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* @_objc_empty_vtable,
// CHECK:   i64 add (i64 ptrtoint ({{.*}}* @_DATA_Foo to i64), i64 1),
// CHECK:   [[FOO]]* ([[TYPE]]*)* @_TC17objc_class_export3Foo6createfMS0_FT_S0_
// CHECK: }

// FIXME: We should export a symbol @"OBJC_CLASS_$_Foo" that points at
// the address point of Foo.

class [objc] Foo {
  var x : Int
  static func [objc] create() -> Foo {
    return new Foo
  }
}