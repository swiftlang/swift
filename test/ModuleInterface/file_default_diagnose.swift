// REQUIRES: swift_feature_DefaultIsolationPerFile

// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name FileDefaultDiagnose \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature DefaultIsolationPerFile
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name FileDefaultDiagnose
// RUN: %FileCheck %s < %t.swiftinterface

// `@diagnose` is an in-file source-warning control. Neither the `default`
// declaration nor the attr should appear in the interface.
// CHECK-NOT: @diagnose
// CHECK-NOT: default @diagnose

default @diagnose(DeprecatedDeclaration, as: ignored)

// CHECK: public func someFunc()
public func someFunc() {}

// CHECK: public class SomeClass
public class SomeClass {
  public init() {}
  public func method() {}
}
