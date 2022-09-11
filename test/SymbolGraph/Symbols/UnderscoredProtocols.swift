// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnderscoredProtocols -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnderscoredProtocols -I %t -pretty-print -output-dir %t

// Make sure that underscored protocols do not appear in public symbol graphs, but their
// requirements do appear on types which conform to them.

// CHECK-NOT: "precise": "s:20UnderscoredProtocols15_HiddenProtocolP",
// CHECK:     "precise": "s:20UnderscoredProtocols10SomeStructV8someFuncyyF",

public protocol _HiddenProtocol {
    func someFunc()
}

public struct SomeStruct {}

extension SomeStruct: _HiddenProtocol {
    public func someFunc() {}
}
