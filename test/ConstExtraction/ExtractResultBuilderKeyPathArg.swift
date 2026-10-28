// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractResultBuilderKeyPathArg.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractResultBuilderKeyPathArg.swiftconstvalues 2>&1 | %FileCheck %s

protocol Node {}

protocol MyProto {}

@resultBuilder
enum NodeBuilder {
  static func buildBlock<Content: Node>(_ content: Content) -> Content {
    content
  }
}

struct Item {
  var `self`: String
  var subtitle: String?
  var id: Int
}

struct Loop<Element, ID, Value, OptValue, SelfIDValue, OptChainValue>: Node {
  init(_ data: [Element], id: KeyPath<Element, ID>, content: KeyPath<Element, Value>, subtitle: KeyPath<Element, OptValue>, selfID: KeyPath<Element, SelfIDValue>, optChain: KeyPath<Element, OptChainValue>) {}
}

struct Crash: MyProto, Node {
  @NodeBuilder
  var body: some Node {
    Loop([Item(self: "One", subtitle: nil, id: 1)], id: \.self, content: \.`self`, subtitle: \.subtitle, selfID: \.self.id, optChain: \.subtitle?.count)
  }
}

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractResultBuilderKeyPathArg.Crash",
// CHECK-NEXT:     "mangledTypeName": "{{.*}}",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}ExtractResultBuilderKeyPathArg.swift",
// CHECK:          "conformances": [
// CHECK:            "ExtractResultBuilderKeyPathArg.MyProto",
// CHECK:            "ExtractResultBuilderKeyPathArg.Node"
// CHECK:          ],
// CHECK-NEXT:     "allConformances": [
// CHECK:            "protocolName": "ExtractResultBuilderKeyPathArg.MyProto",
// CHECK:            "protocolName": "ExtractResultBuilderKeyPathArg.Node"
// CHECK:          ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "body",
// CHECK-NEXT:         "type": "{{.*}}",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "false",
// CHECK-NEXT:         "isComputed": "true",
// CHECK:             "file": "{{.*}}ExtractResultBuilderKeyPathArg.swift",
// CHECK:             "valueKind": "Builder",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractResultBuilderKeyPathArg.NodeBuilder",
// CHECK-NEXT:           "members": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilderKeyPathArg.Loop<ExtractResultBuilderKeyPathArg.Item, ExtractResultBuilderKeyPathArg.Item, Swift.String, Swift.Optional<Swift.String>, Swift.Int, Swift.Optional<Swift.Int>>",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "",
// CHECK-NEXT:                       "type": "Swift.Array<ExtractResultBuilderKeyPathArg.Item>",
// CHECK-NEXT:                       "valueKind": "Array",
// CHECK-NEXT:                       "value": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "valueKind": "InitCall",
// CHECK-NEXT:                           "value": {
// CHECK-NEXT:                             "type": "ExtractResultBuilderKeyPathArg.Item",
// CHECK-NEXT:                             "arguments": [
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "label": "self",
// CHECK-NEXT:                                 "type": "Swift.String",
// CHECK-NEXT:                                 "valueKind": "RawLiteral",
// CHECK-NEXT:                                 "value": "One"
// CHECK-NEXT:                               },
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "label": "subtitle",
// CHECK-NEXT:                                 "type": "Swift.Optional<Swift.String>",
// CHECK-NEXT:                                 "valueKind": "NilLiteral"
// CHECK-NEXT:                               },
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "label": "id",
// CHECK-NEXT:                                 "type": "Swift.Int",
// CHECK-NEXT:                                 "valueKind": "RawLiteral",
// CHECK-NEXT:                                 "value": "1"
// CHECK-NEXT:                               }
// CHECK-NEXT:                             ]
// CHECK-NEXT:                           }
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "id",
// CHECK-NEXT:                       "type": "Swift.KeyPath<ExtractResultBuilderKeyPathArg.Item, ExtractResultBuilderKeyPathArg.Item>",
// CHECK-NEXT:                       "valueKind": "Runtime"
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "content",
// CHECK-NEXT:                       "type": "Swift.KeyPath<ExtractResultBuilderKeyPathArg.Item, Swift.String>",
// CHECK-NEXT:                       "valueKind": "KeyPath",
// CHECK-NEXT:                       "value": {
// CHECK-NEXT:                         "path": "self",
// CHECK-NEXT:                         "rootType": "ExtractResultBuilderKeyPathArg.Item",
// CHECK-NEXT:                         "components": [
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "label": "self",
// CHECK-NEXT:                             "type": "Swift.String"
// CHECK-NEXT:                           }
// CHECK-NEXT:                         ]
// CHECK-NEXT:                       }
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "subtitle",
// CHECK-NEXT:                       "type": "Swift.KeyPath<ExtractResultBuilderKeyPathArg.Item, Swift.Optional<Swift.String>>",
// CHECK-NEXT:                       "valueKind": "KeyPath",
// CHECK-NEXT:                       "value": {
// CHECK-NEXT:                         "path": "subtitle",
// CHECK-NEXT:                         "rootType": "ExtractResultBuilderKeyPathArg.Item",
// CHECK-NEXT:                         "components": [
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "label": "subtitle",
// CHECK-NEXT:                             "type": "Swift.Optional<Swift.String>"
// CHECK-NEXT:                           }
// CHECK-NEXT:                         ]
// CHECK-NEXT:                       }
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "selfID",
// CHECK-NEXT:                       "type": "Swift.KeyPath<ExtractResultBuilderKeyPathArg.Item, Swift.Int>",
// CHECK-NEXT:                       "valueKind": "KeyPath",
// CHECK-NEXT:                       "value": {
// CHECK-NEXT:                         "path": "id",
// CHECK-NEXT:                         "rootType": "ExtractResultBuilderKeyPathArg.Item",
// CHECK-NEXT:                         "components": [
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "label": "id",
// CHECK-NEXT:                             "type": "Swift.Int"
// CHECK-NEXT:                           }
// CHECK-NEXT:                         ]
// CHECK-NEXT:                       }
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "optChain",
// CHECK-NEXT:                       "type": "Swift.KeyPath<ExtractResultBuilderKeyPathArg.Item, Swift.Optional<Swift.Int>>",
// CHECK-NEXT:                       "valueKind": "KeyPath",
// CHECK-NEXT:                       "value": {
// CHECK-NEXT:                         "path": "subtitle.count",
// CHECK-NEXT:                         "rootType": "ExtractResultBuilderKeyPathArg.Item",
// CHECK-NEXT:                         "components": [
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "label": "subtitle",
// CHECK-NEXT:                             "type": "Swift.Optional<Swift.String>"
// CHECK-NEXT:                           },
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "label": "count",
// CHECK-NEXT:                             "type": "Swift.Int"
// CHECK-NEXT:                           }
// CHECK-NEXT:                         ]
// CHECK-NEXT:                       }
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   }
// CHECK-NEXT: ]
