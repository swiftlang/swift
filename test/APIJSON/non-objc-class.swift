// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -typecheck -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution -module-name MyModule -swift-version 5
// RUN: %target-swift-api-extract -o - -pretty-print %t/MyModule.swiftinterface -module-name MyModule -module-cache-path %t | %FileCheck %s

import Foundation

// Need objc data for objc compatible methods.
public class NonObjC {
  @objc public init() {}
  @objc public func testMethod() {}
}

public class NonObjC1 {
  @objc public func testMethod() {}
}

public class NonObjC2 {
  @objc public func testMethod() {}
  public init() {}
}

public class NonObjC3 {
  @objc public init() {}
  public init(fromInt: Int) {}
}

// CHECK:      "interfaces": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule7NonObjC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:     "linkage": "internal",
// CHECK-NEXT:     "super": "",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "testMethod",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule8NonObjC1",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:     "linkage": "internal",
// CHECK-NEXT:     "super": "",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "testMethod",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule8NonObjC2",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:     "linkage": "internal",
// CHECK-NEXT:     "super": "",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "testMethod",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule8NonObjC3",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:     "linkage": "internal",
// CHECK-NEXT:     "super": "",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   }
// CHECK-NEXT: ]

