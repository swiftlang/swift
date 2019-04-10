// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t/InitInheritanceTest.swiftinterface %S/Inputs/InitInheritanceTest.swift -module-name InitInheritanceTest -enable-library-evolution
// RUN: %target-swift-frontend -typecheck %s -I %t -emit-parseable-module-interface-path - | %FileCheck %s

import InitInheritanceTest

// CHECK: public class SubclassOfOpenClassWithDefaultArg : OpenClassWithDefaultArg {
public class SubclassOfOpenClassWithDefaultArg: OpenClassWithDefaultArg {
// CHECK-NEXT: override public init(value: Swift.Int = 9999)
// CHECK-NEXT: @objc deinit
// CHECK-NEXT: }
}