// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractFromExtension.swiftconstvalues -const-gather-protocols-file %t/protocols.json %S/Inputs/ProtocolConformances.swift -primary-file %s
// RUN: cat %t/ExtractFromExtension.swiftconstvalues 2>&1 | %FileCheck %s

extension MyType {
    static let myValue = MyType("it is doable")
}

// CHECK: [
// CHECK-NEXT:  {
// CHECK-NEXT:    "typeName": "ProtocolConformances.MyType",
// CHECK-NEXT:    "mangledTypeName": "20ProtocolConformances6MyTypeV",
// CHECK-NEXT:    "kind": "struct",
// CHECK-NEXT:    "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}Inputs{{/|\\\\}}ProtocolConformances.swift",
// CHECK-NEXT:    "line": 3,
// CHECK-NEXT:    "conformances": [
// CHECK-NEXT:      "ProtocolConformances.MyProto"
// CHECK-NEXT:    ],
// CHECK-NEXT:    "associatedTypeAliases": [],
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "value",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}Inputs{{/|\\\\}}ProtocolConformances.swift",
// CHECK-NEXT:        "line": 4,
// CHECK-NEXT:        "valueKind": "Runtime"
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "myValue",
// CHECK-NEXT:        "type": "ProtocolConformances.MyType",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractFromExtension.swift",
// CHECK-NEXT:        "line": 8,
// CHECK-NEXT:        "valueKind": "InitCall",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "type": "ProtocolConformances.MyType",
// CHECK-NEXT:          "arguments": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "",
// CHECK-NEXT:              "type": "Swift.String",
// CHECK-NEXT:              "valueKind": "RawLiteral",
// CHECK-NEXT:              "value": "it is doable"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK-NEXT:]
