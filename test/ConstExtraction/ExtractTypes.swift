// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractTypes.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractTypes.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}
protocol ContainerType {}

struct TypeA: ContainerType {}
enum TypeB: ContainerType {}
final class TypeC: ContainerType {}

public struct Types : MyProto {
    static var types1: [ContainerType.Type] = [
        TypeA.self,
        TypeB.self,
        TypeC.self
    ]
}

// CHECK:       "label": "types1",
// CHECK-NEXT:  "type": "Swift.Array<ExtractTypes.ContainerType.Type>",
// CHECK:       "valueKind": "Array",
// CHECK-NEXT:  "value": [
// CHECK-NEXT:    {
// CHECK-NEXT:      "valueKind": "Type",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractTypes.TypeA.Type"
// CHECK-NEXT:        "mangledName": "12ExtractTypes5TypeAVm"
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    {
// CHECK-NEXT:      "valueKind": "Type",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractTypes.TypeB.Type"
// CHECK-NEXT:        "mangledName": "12ExtractTypes5TypeBOm"
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    {
// CHECK-NEXT:      "valueKind": "Type",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractTypes.TypeC.Type"
// CHECK-NEXT:        "mangledName": "12ExtractTypes5TypeCCm"
// CHECK-NEXT:      }
// CHECK-NEXT:    }
// CHECK-NEXT:  ]
