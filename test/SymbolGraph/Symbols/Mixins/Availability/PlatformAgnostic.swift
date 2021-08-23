// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PlatformAgnostic -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PlatformAgnostic -I %t -pretty-print -output-dir %t

// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-FS
// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-FP
// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-SO

// CHECK-FS: FutureSwift
@available(swift 99)
public struct FutureSwift {}

// CHECK-FP: FuturePackage
@available(_PackageDescription 99)
public struct FuturePackage {}

// CHECK-SO: SwiftObsolete
@available(swift, obsoleted: 1.0)
public struct SwiftObsolete {}
