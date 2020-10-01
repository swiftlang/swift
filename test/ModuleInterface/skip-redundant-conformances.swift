// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface %s
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.swiftmodule

public protocol ProtocolA : class {}
public protocol ProtocolB: ProtocolA {}
protocol ProtocolC: ProtocolA {}

public class A: ProtocolB {}
public class B: A, ProtocolC {}
