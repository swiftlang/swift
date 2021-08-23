// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/Foo.swiftmodule)
// RUN: echo "public func foo() {}" > %t/Foo.swift

// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo.json

// RUN: %FileCheck %s < %t/Foo.json

// CHECK: "kind": "Root"
// CHECK-NEXT: "name": "TopLevel"
// CHECK-NEXT: "printedName": "TopLevel"
