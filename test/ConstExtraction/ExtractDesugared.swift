// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractLiterals.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractLiterals.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

public struct CommonSugars : MyProto {
    let cane: [String] = ["foo", "bar"]
    let corn: [String: Int] = ["foo" : 1, "bar": 2]
    let demerara: Bool? = nil
}

// CHECK:             "label": "cane",
// CHECK-NEXT:        "type": "Swift.Array<Swift.String>",

// CHECK:             "label": "corn",
// CHECK-NEXT:        "type": "Swift.Dictionary<Swift.String, Swift.Int>",

// CHECK:             "label": "demerara",
// CHECK-NEXT:        "type": "Swift.Optional<Swift.Bool>",
