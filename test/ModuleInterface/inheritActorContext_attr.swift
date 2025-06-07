// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// CHECK-NOT: #if compiler(>=5.3) && $AlwaysInheritActorContext
// CHECK: public func globalTest(@_inheritActorContext _: @Sendable () async -> Swift.Void)
// CHECK-NOT: #endif
public func globalTest(@_inheritActorContext _: @Sendable () async -> Void) {}

// CHECK: #if compiler(>=5.3) && $AlwaysInheritActorContext
// CHECK-NEXT: public func globalTestAlways(@_inheritActorContext(always) _: @Sendable () async -> Swift.Void)
// CHECK-NEXT: #endif
public func globalTestAlways(@_inheritActorContext(always) _: @Sendable () async -> Void) {}

public struct Test {
  // CHECK-NOT: #if compiler(>=5.3) && $AlwaysInheritActorContext
  // CHECK: public init(@_inheritActorContext x: @Sendable () async -> Swift.Int)
  // CHECK-NOT: #endif
  public init(@_inheritActorContext x: @Sendable () async -> Int) {}

  // CHECK: #if compiler(>=5.3) && $AlwaysInheritActorContext
  // CHECK-NEXT: #if compiler(>=5.3) && $SendingArgsAndResults
  // CHECK-NEXT: public init(@_inheritActorContext(always) y: sending () async -> Swift.Void)
  // CHECK-NEXT: #else
  // CHECK-NEXT: public init(@_inheritActorContext(always) y: () async -> Swift.Void)
  // CHECK-NEXT: #endif
  // CHECK-NEXT: #endif
  public init(@_inheritActorContext(always) y: sending () async -> Void) {}

  // CHECK-NOT: #if compiler(>=5.3) && $AlwaysInheritActorContext
  // CHECK: public subscript(@_inheritActorContext _: @Sendable () async -> Swift.Void) -> Swift.Bool {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  // CHECK-NOT: #endif
  public subscript(@_inheritActorContext _: @Sendable () async -> Void) -> Bool { false }

  // CHECK: #if compiler(>=5.3) && $AlwaysInheritActorContext
  // CHECK-NEXT: public subscript(@_inheritActorContext(always) _: @Sendable (Swift.Int) async -> Swift.Void) -> Swift.Bool {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  public subscript(@_inheritActorContext(always) _: @Sendable (Int) async -> Void) -> Bool { false }
}
