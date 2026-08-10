// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractResolvedMemberReferences.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractResolvedMemberReferences.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

// A type outside the extraction protocol list, mimicking e.g. CSCustomAttributeKey
// from an external framework.
struct CustomKey {
    let keyName: String
    init(keyName: String) { self.keyName = keyName }

    // Static stored let — constant initializer
    static let storedKey = CustomKey(keyName: "stored")

    // Static computed property returning an init call
    static var computedKey: CustomKey {
        return CustomKey(keyName: "computed")
    }

    // Static computed property returning a string literal
    static var labelKey: String { return "label" }
}

// Two types that reference each other — cycle detection test
struct CycleA {
    static var value: String { return CycleB.value }
}

struct CycleB {
    static var value: String { return CycleA.value }
}

struct Container: MyProto {
    // Resolved: static let stored property with init call
    var prop1 = CustomKey.storedKey

    // Resolved: static computed property returning an init call
    var prop2 = CustomKey.computedKey

    // Resolved: static computed property returning a string literal
    var prop3 = CustomKey.labelKey

    // Unresolvable: cycle — must fall back to MemberReference
    var prop4 = CycleA.value
}

// CHECK:       "label": "prop1",
// CHECK:       "valueKind": "InitCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "type": "ExtractResolvedMemberReferences.CustomKey",
// CHECK-NEXT:    "arguments": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyName",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "stored"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop2",
// CHECK:       "valueKind": "InitCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "type": "ExtractResolvedMemberReferences.CustomKey",
// CHECK-NEXT:    "arguments": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "label": "keyName",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "computed"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop3",
// CHECK:       "valueKind": "RawLiteral",
// CHECK-NEXT:  "value": "label"

// CHECK:       "label": "prop4",
// CHECK:       "valueKind": "MemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseType": "ExtractResolvedMemberReferences.CycleA",
// CHECK-NEXT:    "memberLabel": "value"
// CHECK-NEXT:  }
