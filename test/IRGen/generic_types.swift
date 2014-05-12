// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: [[A:%C13generic_types1A]] = type <{ [[REF:%swift.refcounted]], [[INT:%Si]] }>
// CHECK: [[INT]] = type <{ i64 }>
// CHECK: [[B:%C13generic_types1B]] = type <{ [[REF:%swift.refcounted]], [[UNSAFE:%VSs13UnsafePointer]] }>
// CHECK: [[C:%C13generic_types1C]] = type
// CHECK: [[D:%C13generic_types1D]] = type

// CHECK: @_TMPdC13generic_types1A = global [[A_METADATA_T:{.*\* } }]] {
// CHECK:   void (i8*, i8*)* [[A_METADATA_FILL:@[a-z0-9_]+]],
// CHECK:   i32 304,
// CHECK:   i16 1,
// CHECK:   i16 16,
// CHECK:   [8 x i8*] zeroinitializer,
// CHECK:   void ([[A]]*)* @_TFC13generic_types1AD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* @_objc_empty_vtable,
// CHECK:   i64 0,
// CHECK:   i32 24,
// CHECK:   i32 7,
// CHECK:   i32 112,
// CHECK:   i32 16,
// CHECK:   %swift.type* null,
// CHECK:   void (%swift.opaque*, [[A]]*)* @_TFC13generic_types1A3run
// CHECK:   %C13generic_types1A* (i64, %C13generic_types1A*)* @_TFC13generic_types1AcU__fMGS0_Q__FT1ySi_GS0_Q__
// CHECK: }
// CHECK: @_TMPdC13generic_types1B = global [[B_METADATA_T:{.* } }]] {
// CHECK:   void (i8*, i8*)* [[B_METADATA_FILL:@[a-z0-9_]+]],
// CHECK:   i32 296,
// CHECK:   i16 1,
// CHECK:   i16 16,
// CHECK:   [8 x i8*] zeroinitializer,
// CHECK:   void ([[B]]*)* @_TFC13generic_types1BD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* @_objc_empty_vtable,
// CHECK:   i64 0,
// CHECK:   i32 24,
// CHECK:   i32 7,
// CHECK:   i32 104,
// CHECK:   i32 16,
// CHECK:   %swift.type* null
// CHECK: }
// CHECK: @_TMPdC13generic_types1C = global [[C_METADATA_T:{.*\* } }]] {
// CHECK:   void ([[C]]*)* @_TFC13generic_types1CD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %swift.type* null,
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* @_objc_empty_vtable,
// CHECK:   i64 0,
// CHECK:   void (%swift.opaque*, [[A]]*)* @_TFC13generic_types1A3run
// CHECK: }
// CHECK: @_TMPdC13generic_types1D = global [[D_METADATA_T:{.*\* } }]] {
// CHECK:   void ([[D]]*)* @_TFC13generic_types1DD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %swift.type* null,
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* @_objc_empty_vtable,
// CHECK:   i64 0,
// CHECK:   void (i64, [[D]]*)* @_TFC13generic_types1D3run
// CHECK: }

// CHECK: define internal void [[A_METADATA_FILL]](i8*, i8*) {
// CHECK: entry:
// CHECK:   [[METADATA_ADDR:%.*]] = bitcast i8* %0 to i64*
// CHECK:   [[ARG_ADDR:%.*]] = bitcast i8* %1 to i64*
// CHECK:   [[METADATA_SLOT_ADDR:%.*]] = getelementptr inbounds i64* [[METADATA_ADDR]], i32 10
// CHECK:   [[ARG_SLOT_ADDR:%.*]] = getelementptr inbounds i64* [[ARG_ADDR]], i32 0
// CHECK:   [[ARG:%.*]] = load i64* [[ARG_SLOT_ADDR]], align 8
// CHECK:   store i64 [[ARG]], i64* [[METADATA_SLOT_ADDR]], align 8
// CHECK:   ret void
// CHECK: }

// CHECK: define internal void [[B_METADATA_FILL]](i8*, i8*) {
// CHECK: entry:
// CHECK:   [[METADATA_ADDR:%.*]] = bitcast i8* %0 to i64*
// CHECK:   [[ARG_ADDR:%.*]] = bitcast i8* %1 to i64*
// CHECK:   [[METADATA_SLOT_ADDR:%.*]] = getelementptr inbounds i64* [[METADATA_ADDR]], i32 10
// CHECK:   [[ARG_SLOT_ADDR:%.*]] = getelementptr inbounds i64* [[ARG_ADDR]], i32 0
// CHECK:   [[ARG:%.*]] = load i64* [[ARG_SLOT_ADDR]], align 8
// CHECK:   store i64 [[ARG]], i64* [[METADATA_SLOT_ADDR]], align 8
// CHECK:   ret void
// CHECK: }

class A<T> {
  var x = 0

  func run(t: T) {}
  init(y : Int) {}
}

class B<T> {
  var ptr : UnsafePointer<T> = nil
  deinit {
    ptr.destroy()
  }
}

class C<T> : A<Int> {}

class D<T> : A<Int> {
  override func run(t: Int) {}
}

struct E<T> {
  var x : Int
  func foo() { bar() }
  func bar() {}
}
