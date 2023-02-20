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

// RUN: %target-swift-emit-module-interface(%t/LibraryPreserveTypesAsWritten.swiftinterface) %s -enable-experimental-concurrency -DLIBRARY -module-name LibraryPreserveTypesAsWritten -module-interface-preserve-types-as-written
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryPreserveTypesAsWritten.swiftinterface) -enable-experimental-concurrency
// RUN: %FileCheck %s < %t/LibraryPreserveTypesAsWritten.swiftinterface
