// RUN: %target-swift-frontend -typecheck -swift-version 6 -enable-library-evolution -module-name isolated_conformance -enable-experimental-feature IsolatedConformances -emit-module-interface-path - %s | %FileCheck %s

// REQUIRES: swift_feature_IsolatedConformances
// REQUIRES: concurrency

public protocol MyProtocol {
  func f()
}

@MainActor
public class MyClass { }

// CHECK: extension isolated_conformance.MyClass : @{{.*}}MainActor isolated_conformance.MyProtocol {
extension MyClass: @MainActor MyProtocol {
  @MainActor public func f() { }
}

extension MyClass: nonisolated Equatable {
  nonisolated public static func ==(lhs: MyClass, rhs: MyClass) -> Bool {
    false
  }
}
