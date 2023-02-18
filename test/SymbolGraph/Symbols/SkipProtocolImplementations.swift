// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name SkipProtocolImplementations -emit-module -emit-module-path %t/SkipProtocolImplementations.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -skip-protocol-implementations
// RUN: %{python} -m json.tool %t/SkipProtocolImplementations.symbols.json %t/SkipProtocolImplementations.formatted.symbols.json
// RUN: %FileCheck %s --input-file %t/SkipProtocolImplementations.formatted.symbols.json

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name SkipProtocolImplementations -emit-module -emit-module-path %t/SkipProtocolImplementations.swiftmodule -emit-module-doc-path %t/SkipProtocolImplementations.swiftdoc
// RUN: %target-swift-symbolgraph-extract -module-name SkipProtocolImplementations -I %t -skip-protocol-implementations -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipProtocolImplementations.symbols.json

// make sure that using `-skip-protocol-implementations` removes the functions from `SomeProtocol` on `SomeStruct`
// CHECK-NOT: s:27SkipProtocolImplementations04SomeB0PAAE9bonusFuncyyF::SYNTHESIZED::s:27SkipProtocolImplementations10SomeStructV
// CHECK-NOT: s:27SkipProtocolImplementations10SomeStructV8someFuncyyF

// CHECK-LABEL: "symbols": [

// SomeStruct.otherFunc() should be present because it has its own doc comment
// CHECK: s:27SkipProtocolImplementations10SomeStructV9otherFuncyyF

// CHECK-LABEL: "relationships": [

// we want to make sure that the conformance relationship itself stays
// CHECK-DAG: conformsTo

// SomeStruct.otherFunc() should be the only one with sourceOrigin information
// CHECK-COUNT-1: sourceOrigin

public protocol SomeProtocol {
    /// Base docs
    func someFunc()

    /// Base docs
    func otherFunc()
}

public extension SomeProtocol {
    func bonusFunc() {}
}

public struct SomeStruct: SomeProtocol {
    public func someFunc() {}

    /// Local docs
    public func otherFunc() {}
}
