// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name DiagnoseAttrTest
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name DiagnoseAttrTest
// RUN: %FileCheck %s < %t.swiftinterface

// The @diagnose attribute should not appear anywhere in the interface.
// CHECK-NOT: @diagnose
// CHECK: public func usesAsDiagnoseWarning() -> Swift
// CHECK-NOT: @diagnose
// CHECK: public func usesAsDiagnoseIgnored() -> Swift
// CHECK-NOT: @diagnose
// CHECK: public struct DiagnoseOnType {
// CHECK-NOT: @diagnose

@available(*, deprecated)
public func deprecatedFunc() -> Int { return 0 }

@diagnose(DeprecatedDeclaration, as: warning)
public func usesAsDiagnoseWarning() -> Int {
    return deprecatedFunc()
}

@diagnose(DeprecatedDeclaration, as: ignored)
public func usesAsDiagnoseIgnored() -> Int {
    return deprecatedFunc()
}

@diagnose(DeprecatedDeclaration, as: ignored)
public struct DiagnoseOnType {}
