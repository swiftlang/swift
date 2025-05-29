// RUN: %empty-directory(%t)
// RUN: echo "[]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractConstantsFromMembersAttribute.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s -enable-experimental-feature ExtractConstantsFromMembers
// RUN: cat %t/ExtractConstantsFromMembersAttribute.swiftconstvalues 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_ExtractConstantsFromMembers

@extractConstantsFromMembers protocol MyProto {}
public struct TestStruct : MyProto {
    let foo = "foo"
    let cane: [String] = ["bar", "baz"]
}

@extractConstantsFromMembers
struct DirectOnStruct {
  var really = true
}

@extractConstantsFromMembers
class DirectOnClass {
  static let answer = 42
}

@extractConstantsFromMembers
enum DirectOnEnum {
  case yes
}

// CHECK:         "typeName": "ExtractConstantsFromMembersAttribute.TestStruct",
// CHECK:         "kind": "struct",
// CHECK:         "conformances": [
// CHECK-NEXT:         "ExtractConstantsFromMembersAttribute.MyProto"
// CHECK-NEXT:    ],
// CHECK:         "properties": [
// CHECK:             "label": "foo",
// CHECK-NEXT:        "type": "Swift.String",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "foo"

// CHECK:             "label": "cane",
// CHECK-NEXT:        "type": "Swift.Array<Swift.String>",
// CHECK:             "valueKind": "Array",


// CHECK:         "typeName": "ExtractConstantsFromMembersAttribute.DirectOnStruct",
// CHECK:         "kind": "struct",
// CHECK:         "properties": [
// CHECK:             "label": "really",
// CHECK-NEXT:        "type": "Swift.Bool",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "true"


// CHECK:         "ExtractConstantsFromMembersAttribute.DirectOnClass",
// CHECK:         "kind": "class",
// CHECK:         "properties": [
// CHECK:             "label": "answer",
// CHECK-NEXT:        "type": "Swift.Int",
// CHECK:             "valueKind": "RawLiteral",
// CHECK:             "value": "42"


// CHECK:         "ExtractConstantsFromMembersAttribute.DirectOnEnum",
// CHECK:         "kind": "enum",
// CHECK:         "cases": [
// CHECK:             "name": "yes"
