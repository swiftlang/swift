// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractLiterals.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractLiterals.swiftconstvalues 2>&1 | %FileCheck %s

struct CoercableThing : ExpressibleByStringLiteral {
    let thing: String
    public init(unicodeScalarLiteral value: String) {
        self.init(stringLiteral: value)
    }

    init(stringLiteral: String) {
        self.thing = stringLiteral
    }
}

protocol MyProto {}
public struct TestStruct : MyProto {
    let foo: CoercableThing = "foo"
    let bar: CoercableThing = CoercableThing("bar")
}

// CHECK:             "label": "foo",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "foo"
// CHECK:             "label": "bar",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "bar"
