// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64
// XFAIL: linux

import Swift

// CHECK: [[A:%C13generic_types1A]] = type <{ [[REF:%swift.refcounted]], [[INT:%Si]] }>
// CHECK: [[INT]] = type <{ i64 }>
// CHECK: [[B:%C13generic_types1B]] = type <{ [[REF:%swift.refcounted]], [[UNSAFE:%VSs20UnsafeMutablePointer]] }>
// CHECK: [[C:%C13generic_types1C]] = type
// FIXME: [[D:%C13generic_types1D]] = type

// CHECK: @_TMPdC13generic_types1A = global [[A_METADATA_T:{.*\* } }]] {
// CHECK:   %swift.type* (%swift.type_pattern*, i8**)* [[A_METADATA_CREATE:@[a-z0-9_]+]],
// CHECK:   i32 336,
// CHECK:   i16 1,
// CHECK:   i16 16,
// CHECK:   [{{[0-9]+}} x i8*] zeroinitializer,
// CHECK:   void ([[A]]*)* @_TFC13generic_types1AD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 1,
// CHECK:   i32 3,
// CHECK:   i32 0,
// CHECK:   i32 24,
// CHECK:   i16 7,
// CHECK:   i16 0,
// CHECK:   i32 144,
// CHECK:   i32 16,
// CHECK:   %swift.type* null,
// CHECK:   void (%swift.opaque*, [[A]]*)* @_TFC13generic_types1A3run
// CHECK:   %C13generic_types1A* (i64, %C13generic_types1A*)* @_TFC13generic_types1AcU__fMGS0_Q__FT1ySi_GS0_Q__
// CHECK: }
// CHECK: @_TMPdC13generic_types1B = global [[B_METADATA_T:{.* } }]] {
// CHECK:   %swift.type* (%swift.type_pattern*, i8**)* [[B_METADATA_CREATE:@[a-z0-9_]+]],
// CHECK:   i32 328,
// CHECK:   i16 1,
// CHECK:   i16 16,
// CHECK:   [{{[0-9]+}} x i8*] zeroinitializer,
// CHECK:   void ([[B]]*)* @_TFC13generic_types1BD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %objc_class* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 1,
// CHECK:   i32 3,
// CHECK:   i32 0,
// CHECK:   i32 24,
// CHECK:   i16 7,
// CHECK:   i16 0,
// CHECK:   i32 136,
// CHECK:   i32 16,
// CHECK:   %swift.type* null
// CHECK: }
// CHECK: @_TMPdC13generic_types1C = global [[C_METADATA_T:{.*\* } }]] {
// CHECK:   void ([[C]]*)* @_TFC13generic_types1CD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0,
// CHECK:   %swift.type* null,
// CHECK:   %swift.opaque* @_objc_empty_cache,
// CHECK:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 1,
// CHECK:   void (%swift.opaque*, [[A]]*)* @_TFC13generic_types1A3run
// CHECK: }
// FIXME: @_TMPdC13generic_types1D = global [[D_METADATA_T:{.*\* } }]] {
// FIXME:   void ([[D]]*)* @_TFC13generic_types1DD,
// FIXME:   i8** @_TWVBo,
// FIXME:   i64 0,
// FIXME:   %swift.type* null,
// FIXME:   %swift.opaque* @_objc_empty_cache,
// FIXME:   %swift.opaque* {{(@_objc_empty_vtable|null)}},
// FIXME:   i64 1,
// FIXME:   void (i64, [[D]]*)* @_TFC13generic_types1D3run
// FIXME: }

// CHECK: define private %swift.type* [[A_METADATA_CREATE]](%swift.type_pattern*, i8**) {
// CHECK: entry:
// CHECK:   [[T0:%.*]] = load i8*, i8** %1
// CHECK:   %T = bitcast i8* [[T0]] to %swift.type*
// CHECK:   [[SUPER:%.*]] = call %objc_class* @swift_getInitializedObjCClass(%objc_class* @"OBJC_CLASS_$_SwiftObject")
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_pattern* %0, i8** %1, %objc_class* [[SUPER]])
// CHECK:   [[SELF_ARRAY:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK:   [[T0:%.*]] = bitcast %swift.type* %T to i8*
// CHECK:   [[T1:%.*]] = getelementptr inbounds i8*, i8** [[SELF_ARRAY]], i32 9
// CHECK:   store i8* [[T0]], i8** [[T1]], align 8
// CHECK:   ret %swift.type* [[METADATA]]
// CHECK: }

// CHECK: define private %swift.type* [[B_METADATA_CREATE]](%swift.type_pattern*, i8**) {
// CHECK: entry:
// CHECK:   [[T0:%.*]] = load i8*, i8** %1
// CHECK:   %T = bitcast i8* [[T0]] to %swift.type*
// CHECK:   [[SUPER:%.*]] = call %objc_class* @swift_getInitializedObjCClass(%objc_class* @"OBJC_CLASS_$_SwiftObject")
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_pattern* %0, i8** %1, %objc_class* [[SUPER]])
// CHECK:   [[SELF_ARRAY:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK:   [[T0:%.*]] = bitcast %swift.type* %T to i8*
// CHECK:   [[T1:%.*]] = getelementptr inbounds i8*, i8** [[SELF_ARRAY]], i32 9
// CHECK:   store i8* [[T0]], i8** [[T1]], align 8
// CHECK:   ret %swift.type* [[METADATA]]
// CHECK: }

class A<T> {
  var x = 0

  func run(t: T) {}
  init(y : Int) {}
}

class B<T> {
  var ptr : UnsafeMutablePointer<T> = nil
  deinit {
    ptr.destroy()
  }
}

class C<T> : A<Int> {}

/* FIXME: We don't properly reabstract D.run to match the indirect calling
   convention of A.run. rdar://problem/19572664
class D<T> : A<Int> {
  override func run(t: Int) {}
}
 */

struct E<T> {
  var x : Int
  func foo() { bar() }
  func bar() {}
}

class ClassA {}
class ClassB {}

// This type is fixed-size across specializations, but it needs to use
// a different implementation in IR-gen so that types match up.
// It just asserts if we get it wrong.
struct F<T: AnyObject> {
  var value: T
}
func testFixed() {
  var a = F(value: ClassA()).value
  var b = F(value: ClassB()).value
}
