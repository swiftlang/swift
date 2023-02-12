// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractRuntimeMetadataAttr.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s -enable-experimental-feature RuntimeDiscoverableAttrs
// RUN: cat %t/ExtractRuntimeMetadataAttr.swiftconstvalues 2>&1 | %FileCheck %s

@runtimeMetadata
struct Flag<T> {
  init(attachedTo: T.Type, _ description: String = "") {}
  init<Args>(attachedTo: (Args) -> T, _ description: String = "") {}
  init<Base>(attachedTo: KeyPath<Base, T>, _ description: String = "") {}
}

protocol MyProto {}

struct A : MyProto {
  @Flag("v1") var v1: String = "foo"
}

// CHECK:    "typeName": "ExtractRuntimeMetadataAttr.A",
// CHECK-NEXT:    "kind": "struct",
// CHECK-NEXT:    "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractRuntimeMetadataAttr.swift",
// CHECK-NEXT:    "line": 16,
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "v1",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractRuntimeMetadataAttr.swift",
// CHECK-NEXT:        "line": 17,
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "foo",
// CHECK-NEXT:        "propertyWrappers": [],
// CHECK-NEXT:        "runtimeMetadataAttributes": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "type": "ExtractRuntimeMetadataAttr.Flag",
// CHECK-NEXT:            "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractRuntimeMetadataAttr.swift",
// CHECK-NEXT:            "line": 17,
// CHECK-NEXT:            "arguments": [
// CHECK-NEXT:              {
// CHECK-NEXT:                "label": "",
// CHECK-NEXT:                "type": "Swift.String",
// CHECK-NEXT:                "valueKind": "RawLiteral",
// CHECK-NEXT:                "value": "v1"
// CHECK-NEXT:              }
// CHECK-NEXT:            ]
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK-NEXT:]
