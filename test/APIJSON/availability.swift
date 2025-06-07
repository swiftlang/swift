// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -typecheck -parse-as-library -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/api.json -target arm64-apple-macos12
// RUN: %validate-json %t/api.json | %FileCheck %s

@available(iOS 13.0, *)
@available(macOS 10.10, *)
@available(tvOS, unavailable)
@available(watchOS 10.0, *)
public class A {}

@available(*, unavailable)
public func callUnavailable() {}

@available(macOS 10.10, *)
@available(*, unavailable)
public func availableOnlyOnActiveOS() {}

@available(tvOS, unavailable)
public func unavailableOnSeperateOS() {}

extension A {
    @available(macOS 12, *)
    public func getA() -> Void {}

    @available(macOS, unavailable)
    public func getUnavailableA() -> Void {}
}

// CHECK:       {
// CHECK-NEXT:     "target": "arm64-apple-macos12",
// CHECK-NEXT:     "globals": [
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule15callUnavailableyyF",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "unavailable": true
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1AC15getUnavailableAyyF",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "unavailable": true
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1AC4getAyyF",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "12"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACMa",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACMm",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACMn",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACMo",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACMu",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACN",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACfD",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule1ACfd",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule23availableOnlyOnActiveOSyyF",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported",
// CHECK-NEXT:             "introduced": "10.10"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:             "name": "_$s8MyModule23unavailableOnSeperateOSyyF",
// CHECK-NEXT:             "access": "public",
// CHECK-NEXT:             "file": "SOURCE_DIR/test/APIJSON/availability.swift",
// CHECK-NEXT:             "linkage": "exported"
// CHECK-NEXT:         }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "interfaces": [],
// CHECK-NEXT:     "categories": [],
// CHECK-NEXT:     "version": "1.0"
// CHECK-NEXT: }
