// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractLiterals.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractLiterals.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}
protocol Bird {}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
struct UnderlyingToOpaquePropertyStruct : MyProto {
    @available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
    var warbler: some Bird {
        Warbler("blue")
    }
}

public struct Warbler : Bird {
    let belly: String = "yellow"
    init(_ color: String = "red") {
        self.belly = color
    }
}

// CHECK:             "valueKind": "InitCall",
// CHECK-NEXT:        "value": {
// CHECK-NEXT:          "type": "ExtractUnderlyingToOpaque.Warbler",
// CHECK-NEXT:          "arguments": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "label": "",
// CHECK-NEXT:              "type": "Swift.String",
// CHECK-NEXT:              "valueKind": "RawLiteral",
// CHECK-NEXT:              "value": "blue"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
