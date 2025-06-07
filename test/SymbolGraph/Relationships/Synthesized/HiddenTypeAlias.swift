// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name HiddenTypeAlias -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name HiddenTypeAlias -I %t -pretty-print -output-dir %t -v
// RUN: %FileCheck %s --input-file %t/HiddenTypeAlias.symbols.json
// RUN: %FileCheck %s --input-file %t/HiddenTypeAlias.symbols.json --check-prefix INNER

// Ensure that public type aliases of effectively-private symbols inherit the child symbols of the
// inner type.

// _InnerType's type name should only appear in quotes like this once, in the declaration for OuterType
// INNER-COUNT-1: "_InnerType"

// CHECK-NOT: someFunc

public protocol SomeProtocol {}

extension SomeProtocol {
    public func bonusFunc() {}
}

public class BaseType {}

public class _InnerType: BaseType, SomeProtocol {
    public func someFunc() {}
}

public typealias OuterType = _InnerType
