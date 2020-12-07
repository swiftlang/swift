// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-concurrency | %target-FileCheck %s
// REQUIRES: concurrency

// rdar_72047158
// XFAIL: CPU=arm64e

// CHECK: %T11actor_class7MyClassC = type <{ %swift.refcounted, [10 x i8*], %TSi }>

// CHECK-objc-LABEL: @"$s11actor_class7MyClassCMm" = global
// CHECK-objc-SAME: %objc_class* @"OBJC_METACLASS_$__TtCs12_SwiftObject"

// CHECK: @"$s11actor_class7MyClassCMf" = internal global
// CHECK-SAME: @"$s11actor_class7MyClassCfD"
// CHECK-objc-SAME: %objc_class* @"OBJC_CLASS_$__TtCs12_SwiftObject"
// CHECK-nonobjc-SAME: %swift.type* null,
//   Flags: uses Swift refcounting
// CHECK-SAME: i32 2,
//   Instance size
// CHECK-64-SAME: i32 104,
// CHECK-32-SAME: i32 52,
//   Alignment mask
// CHECK-SAME: i16 15,
//   Field offset for 'x'
// CHECK-objc-SAME: [[INT]] {{48|96}},

public actor class MyClass {
  public var x: Int
  public init() { self.x = 0 }
}

// CHECK-LABEL: define {{.*}}void @"$s11actor_class7MyClassC7enqueue11partialTasky12_Concurrency012PartialAsyncG0V_tF"
// CHECK:      swift_retain
// CHECK:      [[T0:%.*]] = bitcast %T11actor_class7MyClassC* %1 to {{.*}}*
// CHECK-NEXT: call swiftcc void @swift_defaultActor_enqueue(%swift.job* %0, {{.*}}* [[T0]])

// CHECK-LABEL: define {{.*}}@"$s11actor_class7MyClassC1xSivg"
// CHECK: [[T0:%.*]] = getelementptr inbounds %T11actor_class7MyClassC, %T11actor_class7MyClassC* %0, i32 0, i32 2
// CHECK: [[T1:%.*]] = getelementptr inbounds %TSi, %TSi* [[T0]], i32 0, i32 0
// CHECK: load [[INT]], [[INT]]* [[T1]], align 16

// CHECK-LABEL: define {{.*}}swiftcc %T11actor_class7MyClassC* @"$s11actor_class7MyClassCACycfc"
// FIXME: need to do this initialization!
// CHECK-NOT: swift_defaultActor_initialize
// CHECK-LABEL: ret %T11actor_class7MyClassC*

// CHECK-LABEL: define {{.*}}swiftcc %swift.refcounted* @"$s11actor_class7MyClassCfd"
// FIXME: neeed to do this destruction!
// CHECK-NOT: swift_defaultActor_destroy
// CHECK-LABEL: ret
