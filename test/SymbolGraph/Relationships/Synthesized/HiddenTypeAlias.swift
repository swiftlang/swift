// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name HiddenTypeAlias -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name HiddenTypeAlias -I %t -pretty-print -output-dir %t -v
// RUN: %FileCheck %s --input-file %t/HiddenTypeAlias.symbols.json
// RUN: %FileCheck %s --input-file %t/HiddenTypeAlias.symbols.json --check-prefix INNER

// Ensure that public type aliases of effectively-private symbols inherit the child symbols of the
// inner type.

// _InnerType's type name should only appear in quotes like this once, in the declaration for OuterType
// INNER-COUNT-1: "_InnerType"

// _InnerType.someFunc() as synthesized on OuterType
// CHECK-DAG: "precise": "s:15HiddenTypeAlias06_InnerB0C8someFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a"

// someFunc is a member of OuterType
// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "s:15HiddenTypeAlias06_InnerB0C8someFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a",{{[[:space:]]*}}"target": "s:15HiddenTypeAlias05OuterB0a"

// OuterType conforms to SomeProtocol
// CHECK-DAG: "kind": "conformsTo",{{[[:space:]]*}}"source": "s:15HiddenTypeAlias05OuterB0a",{{[[:space:]]*}}"target": "s:15HiddenTypeAlias12SomeProtocolP"

// OuterType "inherits from" BaseType
// CHECK-DAG: "kind": "inheritsFrom",{{[[:space:]]*}}"source": "s:15HiddenTypeAlias05OuterB0a",{{[[:space:]]*}}"target": "s:15HiddenTypeAlias04BaseB0C"

// bonusFunc as a synthesized member of OuterType
// CHECK-DAG: "precise": "s:15HiddenTypeAlias12SomeProtocolPAAE9bonusFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a",
// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "s:15HiddenTypeAlias12SomeProtocolPAAE9bonusFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a",{{[[:space:]]*}}"target": "s:15HiddenTypeAlias05OuterB0a",

public protocol SomeProtocol {}

extension SomeProtocol {
    public func bonusFunc() {}
}

public class BaseType {}

public class _InnerType: BaseType, SomeProtocol {
    public func someFunc() {}
}

public typealias OuterType = _InnerType
