// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature TargetAttribute -parse-as-library -emit-module-path %t/a.swiftmodule -module-name a -emit-module-interface-path %t/a.swiftinterface -enable-library-evolution -swift-version 5 %s
// RUN: %llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -enable-experimental-feature TargetAttribute -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s
// RUN: %FileCheck --check-prefix INTERFACE-CHECK %s < %t/a.swiftinterface

// REQUIRES: swift_feature_TargetAttribute

// BC-CHECK: <Target_DECL_ATTR

// MODULE-CHECK: @_target("default") @inlinable func inlinableWithTarget(_ x: Int) -> Int
@_target("default")
@inlinable
public func inlinableWithTarget(_ x: Int) -> Int {
    return x + 1
}

// INTERFACE-CHECK: #if compiler(>=5.3) && $TargetAttribute
// INTERFACE-CHECK-NEXT: @_target("default") @inlinable public func inlinableWithTarget(_ x: Swift::Int) -> Swift::Int {
// INTERFACE-CHECK-NEXT: return x + 1
// INTERFACE-CHECK-NEXT: }
// INTERFACE-CHECK-NEXT: #endif
