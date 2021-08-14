// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -enable-library-evolution -enable-experimental-concurrency -emit-module-interface-path %t/Library.swiftinterface -DLIBRARY -module-name Library %s

// REQUIRES: concurrency

#if LIBRARY
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public func fn() async {
  fatalError()
}

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public func reasyncFn(_: () async -> ()) reasync {
  fatalError()
}

// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -I %t

#else
import Library

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
func callFn() async {
  await fn()
}
#endif

// RUN: %FileCheck %s <%t/Library.swiftinterface
// CHECK: // swift-module-flags:{{.*}} -enable-experimental-concurrency
// CHECK: public func fn() async
// CHECK: public func reasyncFn(_: () async -> ()) reasync
