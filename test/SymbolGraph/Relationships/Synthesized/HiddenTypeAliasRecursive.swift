// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name HiddenTypeAliasRecursive -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name HiddenTypeAliasRecursive -I %t -pretty-print -output-dir %t -v
// RUN: %FileCheck %s --input-file %t/HiddenTypeAliasRecursive.symbols.json

// Ensure that mutually-recursive public-private type aliases don't cause infinite recursion in
// SymbolGraphGen when generating symbol graphs. (rdar://145980187)

// HiddenClassB is not referenced by any public symbol, so it shouldn't appear in any symbol graph
// CHECK-NOT: HiddenClassB

// HiddenClassA should only appear one time: in the declaration for PublicAlias
// CHECK-COUNT-1: HiddenClassA
// CHECK-NOT: HiddenClassA

@_documentation(visibility: private)
public class HiddenClassA {
    public typealias ProblematicA = HiddenClassB
    public func funcA() {}
}

@_documentation(visibility: private)
public class HiddenClassB {
    public typealias ProblematicB = HiddenClassA
    public func funcB() {}
}

public typealias PublicAlias = HiddenClassA
