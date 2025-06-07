// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -target %target-future-triple -enable-library-evolution -emit-module-interface-path %t.swiftinterface -module-name t %s -target-min-inlining-version 42 -emit-module -o /dev/null -O -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-experimental-feature MoveOnlyClasses -enable-experimental-feature NoImplicitCopy
// RUN: %FileCheck %s < %t.swiftinterface -check-prefix=CHECK-SWIFTINTERFACE

// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation
// REQUIRES: swift_feature_MoveOnlyClasses
// REQUIRES: swift_feature_NoImplicitCopy
//
// CHECK-SWIFTINTERFACE-NOT: -enable-experimental-feature LayoutStringValueWitnesses
// CHECK-SWIFTINTERFACE-NOT: -enable-experimental-feature LayoutStringValueWitnessesInstantiation
// CHECK-SWIFTINTERFACE: swift-module-flags:
// CHECK-SWIFTINTERFACE-SAME: -enable-experimental-feature MoveOnlyClasses
// CHECK-SWIFTINTERFACE-SAME: -enable-experimental-feature NoImplicitCopy

public func foo() { }
