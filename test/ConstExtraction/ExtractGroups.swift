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
// CHECK-NEXT:        "valueKind": "Array",
// CHECK-NEXT:        "value": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "1"
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "2"
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "3"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "array2",
// CHECK-NEXT:        "type": "[ExtractGroups.Foo]",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "Array",
// CHECK-NEXT:        "value": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "InitCall",
// CHECK-NEXT:            "value": {
// CHECK-NEXT:              "type": "ExtractGroups.Bar",
// CHECK-NEXT:              "arguments": []
// CHECK-NEXT:            }
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "1"
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "\"hi\""
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "array3",
// CHECK-NEXT:        "type": "[ExtractGroups.Bar]",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "Array",
// CHECK-NEXT:        "value": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "valueKind": "InitCall",
// CHECK-NEXT:            "value": {
// CHECK-NEXT:              "type": "ExtractGroups.Bar",
// CHECK-NEXT:              "arguments": []
// CHECK-NEXT:            }
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
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
// CHECK-NEXT:        "type": "(Swift.String, ExtractGroups.Bar)",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "Tuple",
// CHECK-NEXT:        "value": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "type": "Swift.String",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "\"foo\""
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "type": "ExtractGroups.Bar",
// CHECK-NEXT:            "valueKind": "InitCall",
// CHECK-NEXT:            "value": {
// CHECK-NEXT:              "type": "ExtractGroups.Bar",
// CHECK-NEXT:              "arguments": []
// CHECK-NEXT:            }
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "tuple2",
// CHECK-NEXT:        "type": "(lat: Swift.Float, lng: Swift.Float)",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "Tuple",
// CHECK-NEXT:        "value": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "lat",
// CHECK-NEXT:            "type": "Swift.Float",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "42.7"
// CHECK-NEXT:          },
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "lng",
// CHECK-NEXT:            "type": "Swift.Float",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "-73.9"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "tuple3",
// CHECK-NEXT:        "type": "Swift.Void",
// CHECK-NEXT:        "isStatic": "false",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "valueKind": "Tuple",
// CHECK-NEXT:        "value": []
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK-NEXT:]

protocol MyProto {}

public struct Arrays : MyProto {
    let array1: [Int] = [1, 2, 3]
    let array2: [Foo] = [Bar(), 1, "hi"]
    let array3: [Bar] = [Bar()]
}

public struct Dictionaries : MyProto {
    let dict1: [String: Int] = ["One": 1, "Two": 2, "Three": 3]
}

public struct Tuples : MyProto {
    let tuple1: (String, Bar) = ("foo", Bar())
    let tuple2: (lat: Float, lng: Float) = (lat: 42.7, lng: -73.9)
    let tuple3: Void = ()
}

public protocol Foo {}
public struct Bar: Foo {}
extension Int: Foo {}
extension String: Foo {}
