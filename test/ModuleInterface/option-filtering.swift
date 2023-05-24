// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-library-evolution -emit-module-interface-path %t.swiftinterface -module-name t %s -target-min-inlining-version 42 -emit-module -o /dev/null -O -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation
// RUN: %FileCheck %s < %t.swiftinterface -check-prefix=CHECK-SWIFTINTERFACE
//
// CHECK-SWIFTINTERFACE: swift-module-flags-ignorable:
// CHECK-SWIFTINTERFACE-NOT: -enable-experimental-feature LayoutStringValueWitnesses
// CHECK-SWIFTINTERFACE-NOT: -enable-experimental-feature LayoutStringValueWitnessesInstantiation

public func foo() { }
