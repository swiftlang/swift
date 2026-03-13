// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnderscoredProtocols -emit-module -emit-module-path %t/

// RUN: %target-swift-symbolgraph-extract -module-name UnderscoredProtocols -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnderscoredProtocols.symbols.json

// RUN: %target-swift-symbolgraph-extract -module-name UnderscoredProtocols -I %t -skip-protocol-implementations -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnderscoredProtocols.symbols.json

// Make sure that underscored protocols do not appear in public symbol graphs, but their
// requirements do appear on types which conform to them.

// Also ensure that default implementations of underscored protocols appear on implementing types as
// if they were native children of that type. No 'sourceOrigin' information should appear in their
// relationships and no implementation relationships should exist.

// CHECK-DAG: "precise": "s:20UnderscoredProtocols10SomeStructV8someFuncyyF",
// CHECK-DAG: "precise": "s:20UnderscoredProtocols15_HiddenProtocolPAAE9bonusFuncyyF::SYNTHESIZED::s:20UnderscoredProtocols10SomeStructV"

// CHECK-NOT: "precise": "s:20UnderscoredProtocols15_HiddenProtocolP",

// CHECK-NOT: "defaultImplementationOf"
// CHECK-NOT: "requirementOf"
// CHECK-NOT: "optionalRequirementOf"
// CHECK-NOT: "overrides"

// CHECK-NOT: "sourceOrigin"
// CHECK-NOT: "s:20UnderscoredProtocols15_HiddenProtocolPAAE9bonusFuncyyF"

public protocol _HiddenProtocol {
    func someFunc()
}

extension _HiddenProtocol {
    public func bonusFunc() {}
}

public struct SomeStruct {}

extension SomeStruct: _HiddenProtocol {
    public func someFunc() {}
}
