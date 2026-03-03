// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractOpenExistential.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractOpenExistential.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

protocol ExampleProtocol {
    var protocolProperty: String { get }
}

struct ConcreteType: ExampleProtocol {
    let protocolProperty: String = "Concrete implementation"
}

func useExistential(_ example: any ExampleProtocol) -> String {
    return example.protocolProperty
}

public struct External: MyProto {
    static let existentialValue = useExistential(ConcreteType())
}


// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractOpenExistential.External",
// CHECK-NEXT:     "mangledTypeName": "22ExtractOpenExistential8ExternalV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractOpenExistential.swift",
// CHECK-NEXT:     "line": 21,
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "ExtractOpenExistential.MyProto"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "allConformances": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "ExtractOpenExistential.MyProto"
// CHECK-NEXT:         "conformanceDefiningModule": "ExtractOpenExistential"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "existentialValue",
// CHECK-NEXT:         "type": "Swift.String",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractOpenExistential.swift",
// CHECK-NEXT:         "line": 22,
// CHECK-NEXT:         "valueKind": "FunctionCall",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "name": "useExistential",
// CHECK-NEXT:           "arguments": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "label": "",
// CHECK-NEXT:               "type": "any ExtractOpenExistential.ExampleProtocol",
// CHECK-NEXT:               "valueKind": "InitCall",
// CHECK-NEXT:               "value": {
// CHECK-NEXT:                 "type": "ExtractOpenExistential.ConcreteType",
// CHECK-NEXT:                 "arguments": []
// CHECK-NEXT:               }
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   }
// CHECK-NEXT: ]
