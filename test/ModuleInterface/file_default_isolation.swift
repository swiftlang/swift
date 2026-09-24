// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-library-evolution -enable-experimental-feature DefaultIsolationPerFile -module-name FileDefaultIsolation -swift-version 5 -emit-module-interface-path %t/v5.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/v5.swiftinterface -module-name FileDefaultIsolation
// RUN: %FileCheck %s --input-file %t/v5.swiftinterface

// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-library-evolution -enable-experimental-feature DefaultIsolationPerFile -module-name FileDefaultIsolation -swift-version 5 -strict-concurrency=complete -emit-module-interface-path %t/v5_complete.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/v5_complete.swiftinterface -module-name FileDefaultIsolation
// RUN: %FileCheck %s --input-file %t/v5_complete.swiftinterface

// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-library-evolution -enable-experimental-feature DefaultIsolationPerFile -module-name FileDefaultIsolation -swift-version 6 -emit-module-interface-path %t/v6.swiftinterface %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/v6.swiftinterface -module-name FileDefaultIsolation
// RUN: %FileCheck %s --input-file %t/v6.swiftinterface

// REQUIRES: swift_feature_DefaultIsolationPerFile

// The `default` declaration itself must not appear in the emitted interface.
// CHECK-NOT: default @MainActor

// Unlike module-level `-default-isolation MainActor`, @preconcurrency should not be included by default.
// CHECK-NOT: @preconcurrency

default @MainActor

// CHECK: @_Concurrency::MainActor public func defaultedFunc()
public func defaultedFunc() {}

// CHECK: @_Concurrency::MainActor public let defaultedLet
public let defaultedLet = 5

// CHECK: @_Concurrency::MainActor public var defaultedVar
public var defaultedVar = 10

// CHECK: @_Concurrency::MainActor public class DefaultedClass
public class DefaultedClass {
  // CHECK: @_Concurrency::MainActor public init()
  public init() {}
  // CHECK: @_Concurrency::MainActor public func method()
  public func method() {}
}

// CHECK: nonisolated public func explicitNonisolated()
nonisolated public func explicitNonisolated() {}

// CHECK: nonisolated public class NonisolatedClass
public nonisolated class NonisolatedClass {
  // CHECK: nonisolated public init()
  public init() {}

  // CHECK: nonisolated public func method()
  public func method() {}

  // CHECK: @_Concurrency::MainActor public func mainActorMethod()
  @MainActor public func mainActorMethod() {}
}

// CHECK: @_Concurrency::MainActor extension FileDefaultIsolation::NonisolatedClass
extension NonisolatedClass {
  // CHECK: @_Concurrency::MainActor public func extensionMethod()
  public func extensionMethod() {}
}

// CHECK: nonisolated public protocol NonisolatedProto
public nonisolated protocol NonisolatedProto {
  func req()
}

// CHECK: @_Concurrency::MainActor public class Conformer
public class Conformer {}

// CHECK: @_Concurrency::MainActor extension FileDefaultIsolation::Conformer : FileDefaultIsolation::NonisolatedProto
extension Conformer: NonisolatedProto {
  // This is nonisolated due to the explicit nonisolated on NonisolatedProto...
  // CHECK: nonisolated public func req()
  public func req() {}
}

// File defaults shouldn't change visibility.
// CHECK-NOT: internalFunc
// CHECK-NOT: privateFunc
// CHECK-NOT: InternalClass
// CHECK-NOT: internalMember
internal func internalFunc() {}
private func privateFunc() {}
internal class InternalClass {
  func internalMember() {}
}
