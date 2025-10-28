// RUN: %target-swift-emit-ir %s -min-swift-runtime-version 5.0 -O -enable-experimental-feature StandaloneSwiftAvailability | %FileCheck %s

// REQUIRES: swift_feature_StandaloneSwiftAvailability

@_silgen_name("callMeMaybe")
public func callMeMaybe()

// Verify that optimized IR for "if #available(Swift X.Y, *)" is composed of a
// call to the stdlib ABI that returns the current runtime version and an
// integer comparison of that version and the predicate version.

// CHECK-LABEL: define {{.*}}swiftcc void @"$s26availability_swift_runtime15testIfAvailableyyF"()
// CHECK:         [[CURRENT_VERS:%.*]] = tail call swiftcc i32 @"$sSo19_SwiftStdlibVersionasE7currentABvgZ"()
// CHECK:         [[ICMP:%.*]] = icmp {{.*}}
// CHECK:         br i1 [[ICMP]], label %[[TRUE_LABEL:.*]], label %[[FALSE_LABEL:.*]]
// CHECK:       [[TRUE_LABEL]]:
// CHECK:         call {{.*}} @callMeMaybe()
// CHECK:       [[FALSE_LABEL]]:
// CHECK:         ret void
public func testIfAvailable() {
  if #available(Swift 6.2, *) {
    callMeMaybe()
  }
}

// In optimized IR multiple "if #available" checks for the same version should
// only generate a single call to the getter for _SwiftStdlibVersion.current.

// CHECK-LABEL: define {{.*}}swiftcc void @"$s26availability_swift_runtime23testIfAvailableMultipleyyF"()
// CHECK:         call {{.*}} @"$sSo19_SwiftStdlibVersionasE7currentABvgZ"()
// CHECK-NOT:     call {{.*}} @"$sSo19_SwiftStdlibVersionasE7currentABvgZ"()
// CHECK:         icmp
// CHECK:         call {{.*}} @callMeMaybe()
// CHECK:         call {{.*}} @callMeMaybe()
// CHECK:         call {{.*}} @callMeMaybe()
public func testIfAvailableMultiple() {
  if #available(Swift 5.10, *) {
    callMeMaybe()
  }
  if #available(Swift 5.10, *) {
    callMeMaybe()
  }
  if #available(Swift 5.10, *) {
    callMeMaybe()
  }
}
