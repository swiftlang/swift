// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractTopLevelConstants.swiftconstvalues -const-gather-top-level-constant extracted -const-gather-top-level-constant mutable -const-gather-top-level-constant computed -const-gather-top-level-constant destructured -primary-file %s
// RUN: cat %t/ExtractTopLevelConstants.swiftconstvalues 2>&1 | %FileCheck %s

public struct Item {
    var name: String
    static func item(name: String, tags: [String] = []) -> Item {
        Item(name: name)
    }
}

public struct Bag {
    var label: String
    var items: [Item]
}

let notRequested = "ignored"

let extracted = Bag(
    label: "bag",
    items: [.item(name: "first"), .item(name: "second", tags: ["a"])]
)

var mutable = 1
var computed: Int { 42 }

let (destructured, alsoDestructured) = ("x", "y")

struct NotExtracted {
    let unrelated: Int = 0
}

func containsShadowingLocal() {
    let extracted = "local"
    _ = extracted
}

// CHECK:      [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "topLevelConstant",
// CHECK-NEXT:     "label": "extracted",
// CHECK-NEXT:     "type": "ExtractTopLevelConstants.Bag",
// CHECK-NEXT:     "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:     "isStatic": "false",
// CHECK-NEXT:     "isComputed": "false",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractTopLevelConstants.swift",
// CHECK-NEXT:     "line": 20,
// CHECK-NEXT:     "valueKind": "InitCall",
// CHECK-NEXT:     "value": {
// CHECK-NEXT:       "type": "ExtractTopLevelConstants.Bag",
// CHECK-NEXT:       "arguments": [
// CHECK-NEXT:         {
// CHECK-NEXT:           "label": "label",
// CHECK-NEXT:           "type": "Swift.String",
// CHECK-NEXT:           "valueKind": "RawLiteral",
// CHECK-NEXT:           "value": "bag"
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           "label": "items",
// CHECK-NEXT:           "type": "Swift.Array<ExtractTopLevelConstants.Item>",
// CHECK-NEXT:           "valueKind": "Array",
// CHECK-NEXT:           "value": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "valueKind": "StaticFunctionCall",
// CHECK-NEXT:               "value": {
// CHECK-NEXT:                 "type": "ExtractTopLevelConstants.Item",
// CHECK-NEXT:                 "memberLabel": "item",
// CHECK-NEXT:                 "arguments": [
// CHECK-NEXT:                   {
// CHECK-NEXT:                     "label": "name",
// CHECK-NEXT:                     "type": "Swift.String",
// CHECK-NEXT:                     "valueKind": "RawLiteral",
// CHECK-NEXT:                     "value": "first"
// CHECK-NEXT:                   },
// CHECK-NEXT:                   {
// CHECK-NEXT:                     "label": "tags",
// CHECK-NEXT:                     "type": "Swift.Array<Swift.String>",
// CHECK-NEXT:                     "valueKind": "Array",
// CHECK-NEXT:                     "value": []
// CHECK-NEXT:                   }
// CHECK-NEXT:                 ]
// CHECK-NEXT:               }
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "valueKind": "StaticFunctionCall",
// CHECK-NEXT:               "value": {
// CHECK-NEXT:                 "type": "ExtractTopLevelConstants.Item",
// CHECK-NEXT:                 "memberLabel": "item",
// CHECK-NEXT:                 "arguments": [
// CHECK-NEXT:                   {
// CHECK-NEXT:                     "label": "name",
// CHECK-NEXT:                     "type": "Swift.String",
// CHECK-NEXT:                     "valueKind": "RawLiteral",
// CHECK-NEXT:                     "value": "second"
// CHECK-NEXT:                   },
// CHECK-NEXT:                   {
// CHECK-NEXT:                     "label": "tags",
// CHECK-NEXT:                     "type": "Swift.Array<Swift.String>",
// CHECK-NEXT:                     "valueKind": "Array",
// CHECK-NEXT:                     "value": [
// CHECK-NEXT:                       {
// CHECK-NEXT:                         "valueKind": "RawLiteral",
// CHECK-NEXT:                         "value": "a"
// CHECK-NEXT:                       }
// CHECK-NEXT:                     ]
// CHECK-NEXT:                   }
// CHECK-NEXT:                 ]
// CHECK-NEXT:               }
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       ]
// CHECK-NEXT:     }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "topLevelConstant",
// CHECK-NEXT:     "label": "destructured",
// CHECK-NEXT:     "type": "Swift.String",
// CHECK-NEXT:     "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:     "isStatic": "false",
// CHECK-NEXT:     "isComputed": "false",
// CHECK-NEXT:     "file": "{{.*}}ExtractTopLevelConstants.swift",
// CHECK-NEXT:     "line": 28,
// CHECK-NEXT:     "valueKind": "Runtime"
// CHECK-NEXT:   }
// CHECK-NEXT: ]

// CHECK-NOT: "label": "mutable"
// CHECK-NOT: "label": "computed"
