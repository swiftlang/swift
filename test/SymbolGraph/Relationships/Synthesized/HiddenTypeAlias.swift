// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name HiddenTypeAlias -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name HiddenTypeAlias -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/HiddenTypeAlias.symbols.json

// Ensure that public type aliases of effectively-private symbols inherit the child symbols of the
// inner type.

// CHECK-NOT: "OuterType._InnerType"
// CHECK-DAG: "title": "OuterType"

// CHECK-DAG: "precise": "s:15HiddenTypeAlias06_InnerB0V8someFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a"

// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "s:15HiddenTypeAlias06_InnerB0V8someFuncyyF::SYNTHESIZED::s:15HiddenTypeAlias05OuterB0a",{{[[:space:]]*}}"target": "s:15HiddenTypeAlias05OuterB0a"

public struct _InnerType {
    public func someFunc() {}
}

public typealias OuterType = _InnerType
