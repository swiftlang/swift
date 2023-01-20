// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: ExistingType
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Sendable>
@preconcurrency
public struct ExistingType<T: Sendable> : Sendable {}

// CHECK-LABEL: existingClient(arg:)
// CHECK: Canonical generic signature: <τ_0_0>
public func existingClient<T>(arg: T.Type) -> ExistingType<T>? { nil }

public typealias Alias = ExistingType

// CHECK-LABEL: existingClient2(arg:)
// CHECK: Canonical generic signature: <τ_0_0>
public func existingClient2<T>(arg: T.Type) -> Alias<T>? { nil }
