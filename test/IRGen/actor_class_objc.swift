// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-concurrency | %IRGenFileCheck %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar_72047158
// XFAIL: CPU=arm64e

import Foundation

// CHECK: %T16actor_class_objc7MyClassC = type <{ %swift.refcounted, %swift.defaultactor, %TSi }>
// CHECK: %swift.defaultactor = type { [10 x i8*] }

// CHECK-LABEL: @"OBJC_METACLASS_$__TtC16actor_class_objc7MyClass" = global
//   Metaclass is an instance of the root class.
// CHECK-SAME: %objc_class* @"OBJC_METACLASS_$_NSObject",
//   Metaclass superclass is the metaclass of the superclass.
// CHECK-SAME: %objc_class* @"OBJC_METACLASS_$_SwiftNativeNSObject",

// CHECK: @"$s16actor_class_objc7MyClassCMf" = internal global
// CHECK-SAME: @"$s16actor_class_objc7MyClassCfD"
// CHECK-SAME: @"OBJC_METACLASS_$__TtC16actor_class_objc7MyClass"
// CHECK-SAME: @"OBJC_CLASS_$_SwiftNativeNSObject"
//   Flags: uses Swift refcounting
// CHECK-SAME: i32 2,
//   Instance size
// CHECK-64-SAME: i32 104,
// CHECK-32-SAME: i32 52,
//   Alignment mask
// CHECK-64-SAME: i16 15,
// CHECK-32-SAME: i16 7,
//   Field offset for 'x'
// CHECK-64-SAME: i64 96,
// CHECK-32-SAME: i32 48,

public actor class MyClass: NSObject {
  public var x: Int
  public override init() { self.x = 0 }
}

// CHECK-LABEL: define {{.*}}void @"$s16actor_class_objc7MyClassC7enqueue11partialTasky12_Concurrency012PartialAsyncH0V_tF"
// CHECK:      [[T0:%.*]] = bitcast %T16actor_class_objc7MyClassC* %1 to %objc_object*
// CHECK-NEXT: call swiftcc void @swift_defaultActor_enqueue(%swift.job* %0, %objc_object* [[T0]])

// CHECK-LABEL: define {{.*}} @"$s16actor_class_objc7MyClassC1xSivg"
// CHECK: [[T0:%.*]] = getelementptr inbounds %T16actor_class_objc7MyClassC, %T16actor_class_objc7MyClassC* %0, i32 0, i32 2
// CHECK: [[T1:%.*]] = getelementptr inbounds %TSi, %TSi* [[T0]], i32 0, i32 0
// CHECK: load [[INT]], [[INT]]* [[T1]], align

// CHECK-LABEL: define {{.*}}swiftcc %T16actor_class_objc7MyClassC* @"$s16actor_class_objc7MyClassCACycfc"
// CHECK: swift_defaultActor_initialize
// CHECK-LABEL: ret %T16actor_class_objc7MyClassC*

// CHECK: swift_defaultActor_destroy
