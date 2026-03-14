// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/Foo.swiftmodule)
// RUN: echo "public func foo() {}" > %t/Foo.swift
// RUN: echo "public enum DisplayStyle { case tuple, optional, collection }" >> %t/Foo.swift

// RUN: echo "public func bar() {}" > %t/Bar.swift

// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo.json

// RUN: %FileCheck %s < %t/Foo.json

// RUN: %target-swift-frontend -emit-module -module-name Bar -emit-module-path %t/Bar.swiftmodule -emit-abi-descriptor-path %t/Bar.json %t/Bar.swift

// RUN: %FileCheck %s < %t/Bar.json

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Foo~partial1.swiftmodule %t/Foo.swift -module-name FooBar
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Foo~partial2.swiftmodule %t/Bar.swift -module-name FooBar
// RUN: %target-swift-frontend -merge-modules -emit-module -emit-module-path %t/FooBar.swiftmodule %t/Foo~partial1.swiftmodule %t/Foo~partial2.swiftmodule -module-name FooBar -emit-abi-descriptor-path %t/FooBar.json

// RUN: %FileCheck %s < %t/FooBar.json

// RUN: %target-swift-frontend -c -emit-module -emit-module-path %t/FooC.swiftmodule %t/Foo.swift -module-name Foo -emit-abi-descriptor-path %t/Foo-c.json -o %t/Foo.o

// RUN: %FileCheck %s < %t/Foo-c.json

// CHECK: "kind": "Root"
// CHECK-NEXT: "name":
// CHECK-NEXT: "printedName":
// CHECK: "kind": "Function"
