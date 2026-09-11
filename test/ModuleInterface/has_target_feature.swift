// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -target arm64-apple-macosx13.0 -target-cpu apple-a14 -enable-experimental-feature TargetFeaturePredicate -module-name has_target_feature
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name has_target_feature

// REQUIRES: swift_feature_TargetFeaturePredicate
// REQUIRES: OS=macosx && CPU=arm64

// Check that hasTargetFeature is retained rather than evaluated away in inlinable function bodies.

// CHECK-LABEL: func hasDotProd
// CHECK: #if $TargetFeaturePredicate
// CHECK-NEXT: #if _hasTargetFeature("dotprod")
// CHECK-NEXT: return true
// CHECK-NEXT: #else
// CHECK-NEXT: return false
// CHECK-NEXT: #endif
// CHECK-NEXT: #else
// CHECK-NEXT: return false
// CHECK-NEXT: #endif
@inlinable
public func hasDotProd() -> Bool {
#if $TargetFeaturePredicate
#if _hasTargetFeature("dotprod")
  return true
#else
  return false
#endif
#else
  return false
#endif
}
