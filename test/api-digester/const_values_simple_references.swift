// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi.json %S/Inputs/ConstExtraction/SimpleReferences.swift
// RUN: %api-digester -deserialize-sdk -input-paths %t/abi.json -o %t.result

// RUN: %FileCheck %s < %t/abi.json

// CHECK: "offset": 249,
// CHECK-NEXT: "length": 10,
// CHECK-NEXT: "value": "\"abc\"",
// CHECK-NEXT: "decl": "global_str"

// CHECK:      "kind": "IntegerLiteral",
// CHECK-NEXT: "offset": 266,
// CHECK-NEXT: "length": 10,
// CHECK-NEXT: "value": "3",
// CHECK-NEXT: "decl": "global_int"

// CHECK:      "kind": "FloatLiteral",
// CHECK-NEXT: "offset": 283,
// CHECK-NEXT: "length": 12,
// CHECK-NEXT: "value": "3.2",
// CHECK-NEXT: "decl": "global_float"

// CHECK:      "kind": "BooleanLiteral",
// CHECK-NEXT: "offset": 302,
// CHECK-NEXT: "length": 12,
// CHECK-NEXT: "value": "false",
// CHECK-NEXT: "decl": "class_bool"

// CHECK:      "kind": "Array",
// CHECK-NEXT: "offset": 321,
// CHECK-NEXT: "length": 11,
// CHECK-NEXT: "value": "[2, 2, 3]",
// CHECK-NEXT: "decl": "class_arr"

// CHECK: 	   "kind": "Dictionary",
// CHECK-NEXT: "offset": 339,
// CHECK-NEXT: "length": 12,
// CHECK-NEXT: "value": "[(2, 1), (2, 1), (3, 1)]",
// CHECK-NEXT: "decl": "class_dict"
