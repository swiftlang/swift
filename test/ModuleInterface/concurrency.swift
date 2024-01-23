// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-concurrency -DLIBRARY -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-concurrency

// REQUIRES: concurrency

#if LIBRARY
@available(SwiftStdlib 5.5, *)
public func fn() async {
  fatalError()
}

@available(SwiftStdlib 5.5, *)
public func reasyncFn(_: () async -> ()) reasync {
  fatalError()
}

@available(SwiftStdlib 5.5, *)
public func takesSendable(
  _ block: @Sendable @escaping () async throws -> Void
) { }

@available(SwiftStdlib 5.5, *)
public func takesMainActor(
  _ block: @MainActor @escaping () -> Void
) { }

@MainActor(unsafe)
public protocol UnsafeMainProtocol {
  func requirement()
}

public struct InferredUnsafeMainActor: UnsafeMainProtocol {
  public func requirement() {}
}

@preconcurrency @MainActor
public protocol PreconcurrencyMainProtocol {
  func requirement()
}

public struct InferredPreconcurrencyMainActor: PreconcurrencyMainProtocol {
  public func requirement() {}
}

// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -I %t

#else
import Library

@available(SwiftStdlib 5.5, *)
func callFn() async {
  await fn()
}
#endif

// RUN: %FileCheck %s < %t/Library.swiftinterface
// CHECK: // swift-module-flags:{{.*}} -enable-experimental-concurrency
// CHECK: public func fn() async
// CHECK: public func reasyncFn(_: () async -> ()) reasync
// CHECK: public func takesSendable(_ block: @escaping @Sendable () async throws ->
// CHECK: public func takesMainActor(_ block: @escaping @{{_Concurrency.MainActor|MainActor}} () ->

// CHECK:      @_Concurrency.MainActor @preconcurrency public protocol UnsafeMainProtocol {
// CHECK-NEXT:   @_Concurrency.MainActor @preconcurrency func requirement()
// CHECK-NEXT: }

// CHECK:      @_Concurrency.MainActor @preconcurrency public struct InferredUnsafeMainActor :
// CHECK-NEXT:   @_Concurrency.MainActor @preconcurrency public func requirement()
// CHECK-NEXT: }

// CHECK:      @preconcurrency @_Concurrency.MainActor public protocol PreconcurrencyMainProtocol {
// CHECK-NEXT:   @_Concurrency.MainActor @preconcurrency func requirement()
// CHECK-NEXT: }

// CHECK:      @_Concurrency.MainActor @preconcurrency public struct InferredPreconcurrencyMainActor :
// CHECK-NEXT:   @_Concurrency.MainActor @preconcurrency public func requirement()
// CHECK-NEXT: }


// RUN: %target-swift-emit-module-interface(%t/LibraryPreserveTypesAsWritten.swiftinterface) %s -enable-experimental-concurrency -DLIBRARY -module-name LibraryPreserveTypesAsWritten -module-interface-preserve-types-as-written
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryPreserveTypesAsWritten.swiftinterface) -enable-experimental-concurrency
// RUN: %FileCheck %s < %t/LibraryPreserveTypesAsWritten.swiftinterface
