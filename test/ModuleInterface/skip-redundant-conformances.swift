// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo

public protocol ProtocolA : class {}
public protocol ProtocolB: ProtocolA {}
protocol ProtocolC: ProtocolA {}

public class A: ProtocolB {}
public class B: A, ProtocolC {}
