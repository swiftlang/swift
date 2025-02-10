// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractKeyPaths.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractKeyPaths.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

public struct MyType {
    var nested: NestedOne

    struct NestedOne {
        var foo: NestedTwo
    }

    struct NestedTwo {
        var bar: NestedThree
    }

    struct NestedThree {
        var baz: String
    }
}

struct Item {
    var foo: String
    let bar: String
}

public struct KeyPaths: MyProto {
    static let keyPath1: PartialKeyPath<Item> = \Item.foo
    static var keyPath2: KeyPath<Item, String> = \Item.foo
    static var keyPath3: WritableKeyPath<Item, String> = \Item.foo
    static var keyPath4: PartialKeyPath<Item> = \Item.bar
    static var keyPath5: KeyPath<Item, String> = \Item.bar
    static let nestedKeyPath = \MyType.nested.foo.bar.baz
}

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractKeyPaths.KeyPaths",
// CHECK-NEXT:     "mangledTypeName": "15ExtractKeyPaths0bC0V",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK:          "conformances": [
// CHECK-NEXT:      "ExtractKeyPaths.MyProto"
// CHECK-NEXT:     ],
// CHECK-NEXT:    "allConformances": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "protocolName": "ExtractKeyPaths.MyProto"
// CHECK-NEXT:        "conformanceDefiningModule": "ExtractKeyPaths"
// CHECK-NEXT:      }
// CHECK-NEXT:    ],
// CHECK:         "properties": [
// CHECK-NEXT:     {
// CHECK-NEXT:        "label": "keyPath1",
// CHECK-NEXT:        "type": "Swift.PartialKeyPath<ExtractKeyPaths.Item>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:       "line": 31,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "path": "foo",
// CHECK-NEXT:          "rootType": "ExtractKeyPaths.Item",
// CHECK-NEXT:          "components": [
// CHECK-NEXT:           {
// CHECK-NEXT:              "label": "foo",
// CHECK-NEXT:              "type": "Swift.String"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyPath2",
// CHECK-NEXT:        "type": "Swift.KeyPath<ExtractKeyPaths.Item, Swift.String>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 32,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "path": "foo",
// CHECK-NEXT:          "rootType": "ExtractKeyPaths.Item",
// CHECK-NEXT:          "components": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "foo",
// CHECK-NEXT:              "type": "Swift.String"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyPath3",
// CHECK-NEXT:        "type": "Swift.WritableKeyPath<ExtractKeyPaths.Item, Swift.String>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 33,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "path": "foo",
// CHECK-NEXT:          "rootType": "ExtractKeyPaths.Item",
// CHECK-NEXT:          "components": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "foo",
// CHECK-NEXT:              "type": "Swift.String"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyPath4",
// CHECK-NEXT:        "type": "Swift.PartialKeyPath<ExtractKeyPaths.Item>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 34,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "path": "bar",
// CHECK-NEXT:          "rootType": "ExtractKeyPaths.Item",
// CHECK-NEXT:          "components": [
// CHECK-NEXT:            {
// CHECK-NEXT:             "label": "bar",
// CHECK-NEXT:             "type": "Swift.String"
// CHECK-NEXT:           }
// CHECK-NEXT:         ]
// CHECK-NEXT:        }
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyPath5",
// CHECK-NEXT:        "type": "Swift.KeyPath<ExtractKeyPaths.Item, Swift.String>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 35,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "path": "bar",
// CHECK-NEXT:          "rootType": "ExtractKeyPaths.Item",
// CHECK-NEXT:          "components": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "bar",
// CHECK-NEXT:              "type": "Swift.String"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      },
// CHECK-NEXT:       {
// CHECK-NEXT:        "label": "nestedKeyPath",
// CHECK-NEXT:        "type": "Swift.WritableKeyPath<ExtractKeyPaths.MyType, Swift.String>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 36,
// CHECK-NEXT:        "valueKind": "KeyPath",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:         "path": "nested.foo.bar.baz",
// CHECK-NEXT:         "rootType": "ExtractKeyPaths.MyType",
// CHECK-NEXT:         "components": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "label": "nested",
// CHECK-NEXT:               "type": "ExtractKeyPaths.MyType.NestedOne"
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "label": "foo",
// CHECK-NEXT:               "type": "ExtractKeyPaths.MyType.NestedTwo"
// CHECK-NEXT:            },
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "bar",
// CHECK-NEXT:              "type": "ExtractKeyPaths.MyType.NestedThree"
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "label": "baz",
// CHECK-NEXT:               "type": "Swift.String"
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   }
// CHECK-NEXT: ]
