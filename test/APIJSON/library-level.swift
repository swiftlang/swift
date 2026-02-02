// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -parse-as-library  -emit-module -emit-module-path %t/MyModule.swiftmodule -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/none.json -target arm64-apple-macos26
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -parse-as-library  -emit-module -emit-module-path %t/MyModule.swiftmodule -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/api.json -target arm64-apple-macos26 -library-level api
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -parse-as-library  -emit-module -emit-module-path %t/MyModule.swiftmodule -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/spi.json -target arm64-apple-macos26 -library-level spi
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -parse-as-library  -emit-module -emit-module-path %t/MyModule.swiftmodule -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/ipi.json -target arm64-apple-macos26 -library-level ipi
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -parse-as-library  -emit-module -emit-module-path %t/MyModule.swiftmodule -enable-library-evolution -module-name MyModule -swift-version 5 -emit-api-descriptor-path %t/other.json -target arm64-apple-macos26 -library-level other
// RUN: %validate-json %t/api.json | %FileCheck %s --check-prefixes CHECK,API
// RUN: %validate-json %t/none.json | %FileCheck %s --check-prefixes CHECK,NON-API
// RUN: %validate-json %t/spi.json | %FileCheck %s --check-prefixes CHECK,NON-API
// RUN: %validate-json %t/ipi.json | %FileCheck %s --check-prefixes CHECK,NON-API
// RUN: %validate-json %t/other.json | %FileCheck %s --check-prefixes CHECK,NON-API

import Foundation

@_spi(Experimental) public func newUnprovenFunc() {}
@_spi(Experimental) public class MyClass : NSObject {
  @objc public func spiMethod() {}
}

public class MyClass2 : NSObject {
  @_spi(Experimental) @objc public func spiMethod() {}

  @_spi_available(macOS 10.10, tvOS 14.0, *)
  @available(iOS 8.0, *)
  @objc public func spiAvailableMethod() {}
}

@_spi_available(macOS 10.10, tvOS 14.0, *)
@available(iOS 8.0, *)
public func spiAvailableFunc() {}

// CHECK:      {
// CHECK-NEXT:   "target": "arm64-apple-macos26",
// CHECK-NEXT:   "globals": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassC9spiMethodyyFTj",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassC9spiMethodyyFTq",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCACycfC",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCACycfc",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMa",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMn",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMo",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCMu",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCN",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A5ClassCfD",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2C18spiAvailableMethodyyFTj",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "10.10",
// CHECK-NEXT:       "SPIAvailable": true
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2C18spiAvailableMethodyyFTq",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "10.10",
// CHECK-NEXT:       "SPIAvailable": true
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2C9spiMethodyyFTj",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2C9spiMethodyyFTq",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CACycfC",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CACycfc",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMa",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMn",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMo",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMu",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CN",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CfD",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule15newUnprovenFuncyyF",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule16spiAvailableFuncyyF",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "introduced": "10.10",
// CHECK-NEXT:       "SPIAvailable": true
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "interfaces": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_TtC8MyModule7MyClass",
// CHECK-NEXT:       "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "super": "NSObject",
// CHECK-NEXT:       "instanceMethods": [
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "spiMethod",
// CHECK-NEXT:           "access": "private",
// CHECK-NEXT:           "file": "SOURCE_DIR/test/APIJSON/library-level.swift"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "init",
// CHECK-NEXT:           "access": "private",
// CHECK-NEXT:           "file": "SOURCE_DIR/test/APIJSON/library-level.swift"
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       "classMethods": []
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_TtC8MyModule8MyClass2",
// API-NEXT:         "access": "public",
// NON-API-NEXT:     "access": "private",
// CHECK-NEXT:       "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "super": "NSObject",
// CHECK-NEXT:       "instanceMethods": [
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "spiMethod",
// CHECK-NEXT:           "access": "private",
// CHECK-NEXT:           "file": "SOURCE_DIR/test/APIJSON/library-level.swift"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "spiAvailableMethod",
// CHECK-NEXT:           "access": "private",
// CHECK-NEXT:           "file": "SOURCE_DIR/test/APIJSON/library-level.swift",
// CHECK-NEXT:           "introduced": "10.10",
// CHECK-NEXT:           "SPIAvailable": true
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "init",
// API-NEXT:             "access": "public",
// NON-API-NEXT:         "access": "private",
// CHECK-NEXT:           "file": "SOURCE_DIR/test/APIJSON/library-level.swift"
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       "classMethods": []
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "categories": [],
// CHECK-NEXT:   "version": "1.0"
// CHECK-NEXT: }
