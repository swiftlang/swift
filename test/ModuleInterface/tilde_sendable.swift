// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -enable-experimental-feature TildeSendable
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_TildeSendable

// CHECK: #if compiler(>=5.3) && $TildeSendable
// CHECK: public class A : ~Swift::Sendable {
// CHECK: }
// CHECK: #else
// CHECK: public class A {
// CHECK: }
// CHECK: #endif
public class A: ~Sendable {
  public init() {}
}

protocol P {
}

// CHECK: #if compiler(>=5.3) && $TildeSendable
// CHECK: public struct S : ~Swift::Sendable {
// CHECK:   public let x: Swift::Int
// CHECK: }
// CHECK: #else
// CHECK: public struct S {
// CHECK:   public let x: Swift::Int
// CHECK: }
// CHECK: #endif

// CHECK-NOT: extension Library::S : Swift::Sendable {}
public struct S: P, ~Sendable {
  public let x: Int
}

// CHECK: #if compiler(>=5.3) && $TildeSendable
// CHECK: public struct B<T> : ~Swift::Sendable {
// CHECK: }
// CHECK: #else
// CHECK: public struct B<T> {
// CHECK: }
// CHECK: #endif
public struct B<T>: ~Sendable {
}

// CHECK: extension Library::B : Swift::Sendable where T : Swift::Sendable {
// CHECK: }
extension B: Sendable where T: Sendable {
}
