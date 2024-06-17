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

public struct KeyPaths: MyProto {
    static let nestedVariable = \MyType.nested.foo.bar.baz
}

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractKeyPaths.KeyPaths",
// CHECK-NEXT:     "mangledTypeName": "15ExtractKeyPaths0bC0V",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:    "line": 25,
// CHECK-NEXT:    "conformances": [
// CHECK-NEXT:      "ExtractKeyPaths.MyProto"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:    "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:        "label": "nestedVariable",
// CHECK-NEXT:        "type": "Swift.WritableKeyPath<ExtractKeyPaths.MyType, Swift.String>",
// CHECK-NEXT:        "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:        "isStatic": "true",
// CHECK-NEXT:        "isComputed": "false",
// CHECK-NEXT:        "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractKeyPaths.swift",
// CHECK-NEXT:        "line": 26,
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
