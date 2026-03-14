// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractStaticFunctions.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractStaticFunctions.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

enum Bar {
    case one
    case two(item: String)
}

struct Baz {
    static var one: Baz {
        Baz()
    }

    static func two(item: String) -> Baz {
        return Baz()
    }

    static func three() -> Baz {
        return Baz()
    }
}

struct Statics: MyProto {
    var bar1 = Bar.one
    var bar2 = Bar.two(item: "bar")
    var baz1 = Baz.one
    var baz2 = Baz.two(item: "baz")
    var baz3 = Baz.three()
}

// CHECK:       "label": "bar1",
// CHECK-NEXT:  "type": "ExtractStaticFunctions.Bar",
// CHECK:       "valueKind": "Enum",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "name": "one"
// CHECK-NEXT:  }
// CHECK:       "label": "bar2",
// CHECK-NEXT:  "type": "ExtractStaticFunctions.Bar",
// CHECK:       "valueKind": "Enum",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "name": "two",
// CHECK-NEXT:    "arguments": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "item",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "bar"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK:       "label": "baz1",
// CHECK-NEXT:  "type": "ExtractStaticFunctions.Baz",
// CHECK:       "valueKind": "MemberReference"
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseType": "ExtractStaticFunctions.Baz",
// CHECK-NEXT:    "memberLabel": "one"
// CHECK-NEXT:  }
// CHECK:       "label": "baz2",
// CHECK-NEXT:  "type": "ExtractStaticFunctions.Baz",
// CHECK:       "valueKind": "StaticFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "type": "ExtractStaticFunctions.Baz",
// CHECK-NEXT:    "memberLabel": "two",
// CHECK-NEXT:    "arguments": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "item",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "baz"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
// CHECK:       "label": "baz3",
// CHECK-NEXT:  "type": "ExtractStaticFunctions.Baz",
// CHECK:       "valueKind": "StaticFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "type": "ExtractStaticFunctions.Baz",
// CHECK-NEXT:    "memberLabel": "three",
// CHECK-NEXT:    "arguments": []
// CHECK-NEXT:  }
