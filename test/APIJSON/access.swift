// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 5.3
// swift-module-flags: -target x86_64-apple-macos11.0 -enable-objc-interop -enable-library-evolution -module-name MyModule

// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: cp %s %t/MyModule.swiftinterface
// RUN: %target-swift-api-extract -target x86_64-apple-macos11.0 -o - -pretty-print %t/MyModule.swiftinterface -module-name MyModule -module-cache-path %t/ModuleCache | %FileCheck %s

public func publicFunction()
internal func internalFunction()
fileprivate func filePrivateFunction()
private func privateFunction()

// CHECK:        "target":
// CHECK-NEXT:   "globals": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "name": "_$s8MyModule14publicFunctionyyF",
// CHECK-NEXT:       "access": "public",
// CHECK-NEXT:       "file": "/@input/MyModule.swiftinterface",
// CHECK-NEXT:       "linkage": "exported"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   "interfaces": []
