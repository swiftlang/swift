// RUN: %empty-directory(%t)
// RUN: echo "public func foo() {}" > %t/Foo.swift

// RUN: echo "public protocol P { associatedtype A }" > %t/Foo-1.swift
// RUN: echo "public extension P { public func f() where A == Int  {} }" >> %t/Foo-1.swift

// RUN: echo "public protocol P { associatedtype A }" > %t/Foo-2.swift
// RUN: echo "public extension P where A == Int { public func f()  {} }" >> %t/Foo-2.swift

// RUN: %target-swift-frontend -emit-module %t/Foo-1.swift -module-name Foo -emit-module-interface-path %t/Foo1.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Foo-2.swift -module-name Foo -emit-module-interface-path %t/Foo2.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo1.swiftinterface -o %t/Foo1.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo1.json

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo2.swiftinterface -o %t/Foo2.swiftmodule -module-name Foo -emit-abi-descriptor-path %t/Foo2.json

// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Foo1.json -input-paths %t/Foo2.json -abi -o %t/result.txt

// RUN: %FileCheck %s < %t/result.txt

// CHECK: Foo: Func P.f() has mangled name changing from '(extension in Foo):Foo.P.f< where A.A == Swift.Int>() -> ()' to '(extension in Foo):Foo.P< where A.A == Swift.Int>.f() -> ()'
