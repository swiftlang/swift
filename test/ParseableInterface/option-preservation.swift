// RUN: %target-swift-frontend -enable-resilience -emit-parseable-module-interface-path %t.swiftinterface -module-name t %s -emit-module -o /dev/null
// RUN: %FileCheck %s < %t.swiftinterface -check-prefix=CHECK-SWIFTINTERFACE
//
// CHECK-SWIFTINTERFACE: {{swift-module-flags:.* -enable-resilience}}

public func foo() { }
