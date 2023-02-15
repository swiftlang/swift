// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipProtocolImplementations -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipProtocolImplementations -I %t -skip-protocol-implementations -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipProtocolImplementations.symbols.json

// make sure that using `-skip-protocol-implementations` removes the functions from `SomeProtocol` on `SomeStruct`
// CHECK-NOT: s:27SkipProtocolImplementations04SomeB0PAAE9bonusFuncyyF::SYNTHESIZED::s:27SkipProtocolImplementations10SomeStructV
// CHECK-NOT: s:27SkipProtocolImplementations10SomeStructV8someFuncyyF

// the `-skip-protocol-implementations` code should drop any symbol that would get source-origin information
// CHECK-NOT: sourceOrigin

// however, we want to make sure that the conformance relationship itself stays
// CHECK: conformsTo

public protocol SomeProtocol {
    func someFunc()
}

public extension SomeProtocol {
    func bonusFunc() {}
}

public struct SomeStruct: SomeProtocol {
    public func someFunc() {}
}
