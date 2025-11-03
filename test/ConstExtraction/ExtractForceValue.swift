// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractForceValue.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractForceValue.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

class CustomKey {
  init?() {

  }
}
struct MyStruct: MyProto {
  let prop1: CustomKey = CustomKey()!
  let prop2: CustomKey! = CustomKey()
}

// CHECK:          "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "prop1",
// CHECK-NEXT:         "type": "ExtractForceValue.CustomKey",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractForceValue.swift",
// CHECK-NEXT:         "line": 15,
// CHECK-NEXT:         "valueKind": "InitCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "Swift.Optional<ExtractForceValue.CustomKey>",
// CHECK-NEXT:           "arguments": []
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "prop2",
// CHECK-NEXT:         "type": "Swift.Optional<ExtractForceValue.CustomKey>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractForceValue.swift",
// CHECK-NEXT:         "line": 16,
// CHECK-NEXT:         "valueKind": "InitCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "Swift.Optional<ExtractForceValue.CustomKey>",
// CHECK-NEXT:           "arguments": []
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
