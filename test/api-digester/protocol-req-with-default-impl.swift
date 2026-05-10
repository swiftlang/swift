// RUN: %empty-directory(%t)

// RUN: echo "public protocol P {}" > %t/Foo-1.swift
// RUN: echo "public protocol P { func f() }" > %t/Foo-2.swift
// RUN: echo "public extension P { func f() {} }" >> %t/Foo-2.swift

// RUN: %target-swift-frontend -emit-module %t/Foo-1.swift -module-name Foo -emit-module-interface-path %t/Foo1.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Foo-2.swift -module-name Foo -emit-module-interface-path %t/Foo2.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo1.swiftinterface -o %t/Foo1.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo1.json

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo2.swiftinterface -o %t/Foo2.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo2.json

// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Foo1.json -input-paths %t/Foo2.json -abi -o %t/result.txt

// RUN: %FileCheck %s < %t/result.txt

// CHECK-NOT: added as a protocol requirement
