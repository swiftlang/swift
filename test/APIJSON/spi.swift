// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution -module-name MyModule -swift-version 5
// RUN: %target-swift-api-extract -o - -pretty-print %t/MyModule.swiftinterface -module-name MyModule | %FileCheck %s

import Foundation

@_spi(Experimental) public func newUnprovenFunc() {}
@_spi(Experimental) public class MyClass : NSObject {
  public func spiMethod() {}
}

public class MyClass2 : NSObject {
  @_spi(Experimental) public func spiMethod() {}
}

// CHECK:      {
// CHECK-NEXT:   "target":
// CHECK-NEXT:   "globals": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CACycfC",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CACycfc",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMa",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMn",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CMo",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CN",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule0A6Class2CfD",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_OBJC_CLASS_$__TtC8MyModule8MyClass2",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_OBJC_METACLASS_$__TtC8MyModule8MyClass2",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "interfaces": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_TtC8MyModule8MyClass2",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported",
// CHECK-NEXT:       "super": "NSObject",
// CHECK-NEXT:       "instanceMethods": [
// CHECK-NEXT:         {
// CHECK-NEXT:           "name": "init",
// CHECK-NEXT:           "access": "public",
// CHECK-NEXT:           "file": "/@input/MyModule.swiftinterface"
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       "classMethods": []
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "version": "1.0"
// CHECK-NEXT: }
