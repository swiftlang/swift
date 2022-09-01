// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// CHECK: public protocol ProtocolA : AnyObject
public protocol ProtocolA : AnyObject {}
// CHECK: public protocol ProtocolB : Foo.ProtocolA
public protocol ProtocolB: ProtocolA {}
// CHECK-NOT: ProtocolC
protocol ProtocolC: ProtocolA {}

// CHECK: @_hasMissingDesignatedInitializers public class A : Foo.ProtocolB
public class A: ProtocolB {}
// CHECK: @_inheritsConvenienceInitializers @_hasMissingDesignatedInitializers public class B : Foo.A
public class B: A, ProtocolC {}

// CHECK: @_hasMissingDesignatedInitializers public class C
public class C {}
extension C: ProtocolC {}
// CHECK: @_inheritsConvenienceInitializers @_hasMissingDesignatedInitializers public class D : Foo.C
public class D: C {}

// CHECK: extension Foo.C : Foo.ProtocolA {}
// CHECK-NOT: extension Foo.D : Foo.ProtocolA {}
