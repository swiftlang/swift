// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractTypeAliasUnderlyingType.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractTypeAliasUnderlyingType.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

public struct SomeStruct {
    typealias SomeTypeAlias = String
}

public struct Foo: MyProto {
    var name: SomeStruct.SomeTypeAlias?
}

// CHECK:         "properties": [
// CHECK-NEXT:      {
// CHECK-NEXT:       	"label": "name",
// CHECK-NEXT:	        "type": "Swift.Optional<Swift.String>",
