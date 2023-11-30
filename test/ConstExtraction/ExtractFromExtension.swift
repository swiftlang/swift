// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractFromExtension.swiftconstvalues -const-gather-protocols-file %t/protocols.json %S/Inputs/ProtocolConformances.swift -primary-file %s
// RUN: cat %t/ExtractFromExtension.swiftconstvalues 2>&1 | %FileCheck %s

extension MyType {
    static let myValue = MyType("it is doable")
}

// CHECK:       "typeName": "ProtocolConformances.MyType",
// CHECK:       "kind": "struct",
// CHECK:       "conformances": [
// CHECK-NEXT:    "ProtocolConformances.MyProto"
// CHECK-NEXT:  ],
// CHECK:       "properties": [
// CHECK-NEXT:    {
// CHECK-NEXT:      "label": "myValue",
// CHECK:           "type": "ProtocolConformances.MyType",
// CHECK:           "valueKind": "InitCall",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ProtocolConformances.MyType",
// CHECK-NEXT:        "arguments": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "",
// CHECK-NEXT:            "type": "Swift.String",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "it is doable"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      }
// CHECK-NEXT:    }
// CHECK-NEXT:  ]
