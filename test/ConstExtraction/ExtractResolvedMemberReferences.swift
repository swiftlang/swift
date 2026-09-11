// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractResolvedMemberReferences.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractResolvedMemberReferences.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

// A type outside the extraction protocol list
struct CustomKey {
    let keyName: String
    init(keyName: String) { self.keyName = keyName }

    static let storedKey = CustomKey(keyName: "stored")

    static var computedKey: CustomKey {
        CustomKey(keyName: "computed")
    }

    static var labelKey: String { "label" }

    static var dynamicLabelKey: String {  Int.random(in: 0...100) > 50 ? "dynamic1" : "dynamic2" }
    static var dyanmicComputedKey: CustomKey { Int.random(in: 0...100) > 50 ? CustomKey(keyName: "dynamic1") : CustomKey(keyName: "dynamic2") }
    static let dynamicStoredKey: CustomKey = Int.random(in: 0...100) > 50 ? CustomKey(keyName: "a") : CustomKey(keyName: "b")
    static let interpolatedStringKey: String = "interpolated \(Self.labelKey)"
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

    // Unresolvable: dynamic values — must fall back to MemberReference
    var prop5 = CustomKey.dynamicLabelKey
    var prop6 = CustomKey.dyanmicComputedKey
    var prop7 = CustomKey.dynamicStoredKey

    // Resolved: static stored property containing an interpolated string literal with a member reference
    var prop8 = CustomKey.interpolatedStringKey
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

// CHECK:       "label": "prop5",
// CHECK:       "valueKind": "MemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseType": "ExtractResolvedMemberReferences.CustomKey",
// CHECK-NEXT:    "memberLabel": "dynamicLabelKey"
// CHECK-NEXT:  }

// CHECK:       "label": "prop6",
// CHECK:       "valueKind": "MemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseType": "ExtractResolvedMemberReferences.CustomKey",
// CHECK-NEXT:    "memberLabel": "dyanmicComputedKey"
// CHECK-NEXT:  }

// CHECK:       "label": "prop7",
// CHECK:       "valueKind": "MemberReference",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseType": "ExtractResolvedMemberReferences.CustomKey",
// CHECK-NEXT:    "memberLabel": "dynamicStoredKey"
// CHECK-NEXT:  }

// CHECK:       "label": "prop8",
// CHECK:       "valueKind": "InterpolatedStringLiteral",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "segments": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "interpolated "
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": "label"
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "valueKind": "RawLiteral",
// CHECK-NEXT:        "value": ""
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
