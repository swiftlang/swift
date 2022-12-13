// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractGroups.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractGroups.swiftconstvalues 2>&1 | %FileCheck %s

// CHECK: [
// CHECK-NEXT:  {
// CHECK-NEXT:    "typeName": "ExtractGroups.Arrays",
// CHECK-NEXT:    "kind": "struct",
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "array1",
// CHECK-NEXT:        "type": "[Swift.Int]",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "[1, 2, 3]"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "typeName": "ExtractGroups.Dictionaries",
// CHECK-NEXT:    "kind": "struct",
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "dict1",
// CHECK-NEXT:        "type": "[Swift.String : Swift.Int]",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "[(\"One\", 1), (\"Two\", 2), (\"Three\", 3)]"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "typeName": "ExtractGroups.Tuples",
// CHECK-NEXT:    "kind": "struct",
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "tuple1",
// CHECK-NEXT:        "type": "(Swift.Int, Swift.Float)",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "(42, 6.6)"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK-NEXT:]

protocol MyProto {}

public struct Arrays : MyProto {
    let array1: [Int] = [1, 2, 3]
}

public struct Dictionaries : MyProto {
    let dict1: [String: Int] = ["One": 1, "Two": 2, "Three": 3]
}

public struct Tuples : MyProto {
    let tuple1: (Int, Float) = (42, 6.6)
}
