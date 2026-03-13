// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractVariadicArgs.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractVariadicArgs.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

public enum SimpleEnum: MyProto {
    static func varargs(args: Int...) -> [Int] {
        args
    }
}

public struct Driver: MyProto {
    static let someInts = SimpleEnum.varargs(args: 1, 2, 3, 4, 5)
}

// CHECK: "typeName": "ExtractVariadicArgs.Driver",
// CHECK: "associatedTypeAliases": [],
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "someInts",
// CHECK-NEXT:        "type": "Swift.Array<Swift.Int>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractVariadicArgs.swift",
// CHECK-NEXT:        "line": 16,
// CHECK-NEXT:        "valueKind": "StaticFunctionCall",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "type": "Swift.Array<Swift.Int>",
// CHECK-NEXT:          "memberLabel": "varargs",
// CHECK-NEXT:          "arguments": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "args",
// CHECK-NEXT:              "type": "Swift.Int...",
// CHECK-NEXT:              "valueKind": "Array",
// CHECK-NEXT:              "value": [
// CHECK-NEXT:                {
// CHECK-NEXT:                  "valueKind": "RawLiteral",
// CHECK-NEXT:                  "value": "1"
// CHECK-NEXT:                },
// CHECK-NEXT:                {
// CHECK-NEXT:                  "valueKind": "RawLiteral",
// CHECK-NEXT:                  "value": "2"
// CHECK-NEXT:                },
// CHECK-NEXT:                {
// CHECK-NEXT:                  "valueKind": "RawLiteral",
// CHECK-NEXT:                  "value": "3"
// CHECK-NEXT:                },
// CHECK-NEXT:                {
// CHECK-NEXT:                  "valueKind": "RawLiteral",
// CHECK-NEXT:                  "value": "4"
// CHECK-NEXT:                },
// CHECK-NEXT:                {
// CHECK-NEXT:                  "valueKind": "RawLiteral",
// CHECK-NEXT:                  "value": "5"
// CHECK-NEXT:                }
// CHECK-NEXT:              ]
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
