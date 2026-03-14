// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractLiterals.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractLiterals.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}
struct InjectablePropertyStruct : MyProto {
    let init1 = Bat(buz: "hello", fuz: 4)
}

public struct Bat {
    let buz: String?
    let fuz: Int

    init(buz: String? = "", fuz: Int = 0) {
        self.buz = buz
        self.fuz = fuz
    }
}

// CHECK:      "arguments": [
// CHECK-NEXT: {
// CHECK-NEXT:   "label": "buz",
// CHECK-NEXT:   "type": "Swift.Optional<Swift.String>",
// CHECK-NEXT:   "valueKind": "RawLiteral",
// CHECK-NEXT:   "value": "hello"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "label": "fuz",
// CHECK-NEXT:   "type": "Swift.Int",
// CHECK-NEXT:   "valueKind": "RawLiteral",
// CHECK-NEXT:   "value": "4"
// CHECK-NEXT: }
// CHECK-NEXT: ]
