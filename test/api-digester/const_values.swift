// RUN: %empty-directory(%t)

// RUN: echo "public func foo() { let a = \"abc\" }" > %t/t1.swift
// RUN: echo "public func bar() { let a = \"def\" }" > %t/t2.swift
// RUN: %target-swift-frontend -emit-module %t/t1.swift %t/t2.swift -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi.json
// RUN: %api-digester -deserialize-sdk -input-paths %t/abi.json -o %t.result

// RUN: %FileCheck %s < %t/abi.json

// CHECK: "kind": "StringLiteral"
// CHECK: "value": "\"abc\""
// CHECK: "kind": "StringLiteral"
// CHECK: "value": "\"def\""
