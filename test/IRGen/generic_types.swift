// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// REQUIRES: CPU=x86_64

// CHECK: [[A:%T13generic_types1AC]] = type <{ [[REF:%swift.refcounted]], [[INT:%TSi]] }>
// CHECK: [[INT]] = type <{ i64 }>
// CHECK: [[B:%T13generic_types1BC]] = type <{ [[REF:%swift.refcounted]], [[UNSAFE:%TSp]] }>
// CHECK: [[C:%T13generic_types1CC]] = type
// CHECK: [[D:%T13generic_types1DC]] = type

// CHECK-LABEL: @"$S13generic_types1ACMI" = internal global [16 x i8*] zeroinitializer, align 8

// CHECK-LABEL: @"$S13generic_types1ACMn" = hidden constant
// CHECK-SAME:   @"$S13generic_types1ACMa"
// CHECK-SAME:   @"$S13generic_types1ACMi"
// CHECK-SAME:   @"$S13generic_types1ACMI"

// CHECK-LABEL: @"$S13generic_types1ACMP" = internal constant
// CHECK-SAME:   void ([[A]]*)* @"$S13generic_types1ACfD",
// CHECK-SAME:   i8** @"$SBoWV",
// CHECK-SAME:   i64 0,
// CHECK-SAME:   %swift.type* null,
// CHECK-native-SAME: %swift.opaque* null,
// CHECK-objc-SAME:   %swift.opaque* @_objc_empty_cache,
// CHECK-SAME:   %swift.opaque* null,
// CHECK-SAME:   i64 {{1|2}},
// CHECK-SAME:   i32 {{3|2}},
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 24,
// CHECK-SAME:   i16 7,
// CHECK-SAME:   i16 0,
// CHECK-SAME:   i32 152,
// CHECK-SAME:   i32 16,
// -- nominal type descriptor
// CHECK-SAME:   @"$S13generic_types1ACMn",
// -- ivar destroyer
// CHECK-SAME:   i8* null
// CHECK-SAME: }

// CHECK-LABEL: @"$S13generic_types1BCMI" = internal global [16 x i8*] zeroinitializer, align 8

// CHECK-LABEL: @"$S13generic_types1BCMn" = hidden constant
// CHECK-SAME:   @"$S13generic_types1BCMa"
// CHECK-SAME:   @"$S13generic_types1BCMi"
// CHECK-SAME:   @"$S13generic_types1BCMI"

// CHECK-LABEL: @"$S13generic_types1BCMP" = internal constant
// CHECK-SAME:   void ([[B]]*)* @"$S13generic_types1BCfD",
// CHECK-SAME:   i8** @"$SBoWV",
// CHECK-SAME:   i64 0,
// CHECK-SAME:   %swift.type* null,
// CHECK-native-SAME: %swift.opaque* null,
// CHECK-objc-SAME:   %swift.opaque* @_objc_empty_cache,
// CHECK-SAME:   %swift.opaque* null,
// CHECK-SAME:   i64 {{1|2}},
// CHECK-SAME:   i32 {{3|2}},
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 24,
// CHECK-SAME:   i16 7,
// CHECK-SAME:   i16 0,
// CHECK-SAME:   i32 144,
// CHECK-SAME:   i32 16,
// -- nominal type descriptor
// CHECK-SAME:   @"$S13generic_types1BCMn",
// -- ivar destroyer
// CHECK-SAME:   i8* null
// CHECK-SAME: }

// CHECK-LABEL: @"$S13generic_types1CCMP" = internal constant
// CHECK-SAME:   void ([[C]]*)* @"$S13generic_types1CCfD",
// CHECK-SAME:   i8** @"$SBoWV",
// CHECK-SAME:   i64 0,
// CHECK-SAME:   %swift.type* null,
// CHECK-native-SAME: %swift.opaque* null,
// CHECK-objc-SAME:   %swift.opaque* @_objc_empty_cache,
// CHECK-SAME:   %swift.opaque* null,
// CHECK-SAME:   i64 {{1|2}},
// CHECK-SAME:   i32 {{3|2}},
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 24,
// CHECK-SAME:   i16 7,
// CHECK-SAME:   i16 0,
// CHECK-SAME:   i32 160,
// CHECK-SAME:   i32 16,
// -- nominal type descriptor
// CHECK-SAME:   @"$S13generic_types1CCMn",
// -- ivar destroyer
// CHECK-SAME:   i8* null
// CHECK-SAME: }

// CHECK-LABEL: @"$S13generic_types1DCMP" = internal constant
// CHECK-SAME:   void ([[D]]*)* @"$S13generic_types1DCfD",
// CHECK-SAME:   i8** @"$SBoWV",
// CHECK-SAME:   i64 0,
// CHECK-SAME:   %swift.type* null,
// CHECK-native-SAME: %swift.opaque* null,
// CHECK-objc-SAME:   %swift.opaque* @_objc_empty_cache,
// CHECK-SAME:   %swift.opaque* null,
// CHECK-SAME:   i64 {{1|2}},
// CHECK-SAME:   i32 {{3|2}},
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 24,
// CHECK-SAME:   i16 7,
// CHECK-SAME:   i16 0,
// CHECK-SAME:   i32 160,
// CHECK-SAME:   i32 16,
// -- nominal type descriptor
// CHECK-SAME:   @"$S13generic_types1DCMn",
// -- ivar destroyer
// CHECK-SAME:   i8* null
// CHECK-SAME: }

// CHECK-LABEL: define{{( protected)?}} internal %swift.type* @"$S13generic_types1ACMi"(%swift.type_descriptor*, i8**) {{.*}} {
// CHECK:   [[T0:%.*]] = bitcast i8** %1 to %swift.type**
// CHECK:   %T = load %swift.type*, %swift.type** [[T0]],
// CHECK-native: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** bitcast ({{.*}}* @"$S13generic_types1ACMP" to i8**), i64 96, i64 16, i8** %1, %objc_class* null, i64 7)
// CHECK-objc:   [[T0:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$__TtCs12_SwiftObject"
// CHECK-objc:   [[SUPER:%.*]] = call %objc_class* @swift_getInitializedObjCClass(%objc_class* [[T0]])
// CHECK-objc:   [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** bitcast ({{.*}}* @"$S13generic_types1ACMP" to i8**), i64 280, i64 200, i8** %1, %objc_class* [[SUPER]], i64 7)
// CHECK:   [[SELF_ARRAY:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK:   [[T1:%.*]] = getelementptr inbounds i8*, i8** [[SELF_ARRAY]], i64 10
// CHECK:   [[T0:%.*]] = bitcast %swift.type* %T to i8*
// CHECK:   store i8* [[T0]], i8** [[T1]], align 8
// CHECK:   ret %swift.type* [[METADATA]]
// CHECK: }

// CHECK-LABEL: define{{( protected)?}} internal %swift.type* @"$S13generic_types1BCMi"(%swift.type_descriptor*, i8**) {{.*}} {
// CHECK:   [[T0:%.*]] = bitcast i8** %1 to %swift.type**
// CHECK:   %T = load %swift.type*, %swift.type** [[T0]],
// CHECK-native: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** bitcast ({{.*}}* @"$S13generic_types1BCMP" to i8**), i64 96, i64 16, i8** %1, %objc_class* null, i64 6)
// CHECK-objc:   [[T0:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$__TtCs12_SwiftObject"
// CHECK-objc:   [[SUPER:%.*]] = call %objc_class* @swift_getInitializedObjCClass(%objc_class* [[T0]])
// CHECK-objc:   [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** bitcast ({{.*}}* @"$S13generic_types1BCMP" to i8**), i64 280, i64 200, i8** %1, %objc_class* [[SUPER]], i64 6)
// CHECK:   [[SELF_ARRAY:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK:   [[T1:%.*]] = getelementptr inbounds i8*, i8** [[SELF_ARRAY]], i64 10
// CHECK:   [[T0:%.*]] = bitcast %swift.type* %T to i8*
// CHECK:   store i8* [[T0]], i8** [[T1]], align 8
// CHECK:   ret %swift.type* [[METADATA]]
// CHECK: }

class A<T> {
  var x = 0

  func run(_ t: T) {}
  init(y : Int) {}
}

class B<T> {
  var ptr : UnsafeMutablePointer<T>
  init(ptr: UnsafeMutablePointer<T>) {
    self.ptr = ptr
  }
  deinit {
    ptr.deinitialize(count: 1)
  }
}

class C<T> : A<Int> {}

class D<T> : A<Int> {
  override func run(_ t: Int) {}
}

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
