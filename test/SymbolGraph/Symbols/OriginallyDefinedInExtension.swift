// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/macos)
// RUN: %empty-directory(%t/ios)

// RUN: %target-swift-frontend -target %target-cpu-apple-macos %s -module-name OriginallyDefinedInExtension -emit-module -emit-module-path %t/macos/OriginallyDefinedInExtension.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/macos/
// RUN: %FileCheck %s --input-file %t/macos/OriginallyDefinedInExtension.symbols.json
// RUN: %target-swift-frontend -target %target-cpu-apple-ios-simulator %s -module-name OriginallyDefinedInExtension -emit-module -emit-module-path %t/ios/OriginallyDefinedInExtension.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ios/
// RUN: %FileCheck %s --input-file %t/ios/OriginallyDefinedInExtension.symbols.json

// CHECK:   "precise":"s:28OriginallyDefinedInExtension12SimpleStructV05InnerF0V"

// REQUIRES: SWIFT_SDK=osx
// REQUIRES: SWIFT_SDK=ios_simulator

@available(macOS 10.8, *)
@_originallyDefinedIn(module: "another", macOS 11.0)
public struct SimpleStruct {}

@available(macOS 12.0, iOS 13.0, *)
public extension SimpleStruct {
    struct InnerStruct {}
}
