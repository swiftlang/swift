// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -module-name Test

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -module-name Test

// RUN: %FileCheck %s < %t/Test.swiftinterface

// REQUIRES: swift_feature_CustomAvailability

import Oceans // re-exports Rivers

// CHECK:      #if compiler(>=5.3) && $CustomAvailability
// CHECK-NEXT: @available(Colorado)
// CHECK-NEXT: public func availableInColorado()
// CHECK-NEXT: #endif
@available(Colorado)
public func availableInColorado() { }

// CHECK:      #if compiler(>=5.3) && $CustomAvailability
// CHECK-NEXT: @available(Arctic, unavailable)
// CHECK-NEXT: @available(Pacific)
// CHECK-NEXT: public func unavailableInArcticButAvailableInPacific()
// CHECK-NEXT: #endif
@available(Arctic, unavailable)
@available(Pacific)
public func unavailableInArcticButAvailableInPacific() { }
