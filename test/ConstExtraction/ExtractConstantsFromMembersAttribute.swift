// RUN: %empty-directory(%t)
// RUN: echo "[]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractConstantsFromMembersAttribute.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s -enable-experimental-feature ExtractConstantsFromMembers
// RUN: cat %t/ExtractConstantsFromMembersAttribute.swiftconstvalues 2>&1 | %FileCheck %s

// REQUIRES: asserts

@extractConstantsFromMembers protocol MyProto {}
public struct TestStruct : MyProto {
    let foo = "foo"
    let cane: [String] = ["bar", "baz"]
}

// CHECK:             "label": "foo",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "foo"

// CHECK:             "label": "cane",
// CHECK-NEXT:        "type": "Swift.Array<Swift.String>",
