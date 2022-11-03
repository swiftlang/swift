// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DocumentationAttr -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DocumentationAttr -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DocumentationAttr.symbols.json --check-prefix PUBLIC

// RUN: %target-swift-symbolgraph-extract -module-name DocumentationAttr -I %t -pretty-print -output-dir %t -minimum-access-level internal
// RUN: %FileCheck %s --input-file %t/DocumentationAttr.symbols.json --check-prefix INTERNAL

// RUN: %target-swift-symbolgraph-extract -module-name DocumentationAttr -I %t -pretty-print -output-dir %t -minimum-access-level private
// RUN: %FileCheck %s --input-file %t/DocumentationAttr.symbols.json --check-prefix PRIVATE

// This test is a mirror of SkipsPublicUnderscore.swift, but using `@_documentation`
// instead of underscored names.

public protocol PublicProtocol {}

// PUBLIC-NOT: ShouldntAppear
// INTERNAL-DAG: ShouldntAppear
// PRIVATE-DAG: ShouldntAppear

@_documentation(visibility: internal) public struct ShouldntAppear: PublicProtocol {
    public struct InnerShouldntAppear {}
}

public class SomeClass {
    // PUBLIC-NOT: internalVar
    // INTERNAL-NOT: internalVar
    // PRIVATE-DAG: internalVar
    @_documentation(visibility: private) internal var internalVar: String = ""
}

@_documentation(visibility: internal) public protocol ProtocolShouldntAppear {}

public struct PublicStruct: ProtocolShouldntAppear {
    @_documentation(visibility: internal) public struct InnerShouldntAppear {}
}

// The presence of `@_documentation(visibility: public)` should cause SymbolGraphGen to include an
// underscored enum case.

// PUBLIC-DAG: _ShouldAppear

public enum PublicEnum {
    case RegularCase
    case _ShouldntAppear
    @_documentation(visibility: public) case _ShouldAppear
}
