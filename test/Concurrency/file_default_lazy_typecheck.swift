// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/eager)
// RUN: %empty-directory(%t/lazy)

// RUN: %target-swift-frontend -emit-module -module-name Lib -swift-version 6 -enable-library-evolution -disable-availability-checking -enable-experimental-feature DefaultIsolationPerFile -o %t/eager/Lib.swiftmodule -emit-module-interface-path %t/eager/Lib.swiftinterface %t/lib.swift
// RUN: %target-swift-frontend -emit-module -module-name Lib -swift-version 6 -enable-library-evolution -disable-availability-checking -enable-experimental-feature DefaultIsolationPerFile -experimental-lazy-typecheck -o %t/lazy/Lib.swiftmodule -emit-module-interface-path %t/lazy/Lib.swiftinterface %t/lib.swift

// RUN: diff %t/eager/Lib.swiftinterface %t/lazy/Lib.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/lazy/Lib.swiftinterface) -module-name Lib

// RUN: %FileCheck %t/lib.swift --input-file %t/lazy/Lib.swiftinterface

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -swift-version 6 -disable-availability-checking -I %t/eager %t/client.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -swift-version 6 -disable-availability-checking -I %t/lazy %t/client.swift

// Make sure the default doesn't force typechecking...
// RUN: %target-swift-frontend -emit-module -emit-module-path /dev/null -module-name Lib -swift-version 6 -enable-library-evolution -parse-as-library -disable-availability-checking -enable-experimental-feature DefaultIsolationPerFile -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls -debug-forbid-typecheck-prefix NoTypecheck %t/lib.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- lib.swift

using @MainActor

// CHECK: @_Concurrency::MainActor public func libFunc()
public func libFunc() {}

// CHECK: @_Concurrency::MainActor public class LibClass
public class LibClass {
  // CHECK: @_Concurrency::MainActor public init()
  public init() {}
  // CHECK: @_Concurrency::MainActor public func method()
  public func method() {}
}

// CHECK: @_Concurrency::MainActor public struct LibStruct
public struct LibStruct {
  // CHECK: @_Concurrency::MainActor public init()
  public init() {}
  // CHECK: @_Concurrency::MainActor public func method()
  public func method() {}
}

// CHECK: @_Concurrency::MainActor public protocol LibProto
public protocol LibProto {
  // CHECK: @_Concurrency::MainActor func requirement()
  func requirement()
}

// CHECK: @_Concurrency::MainActor extension Lib::LibStruct
extension LibStruct {
  // CHECK: @_Concurrency::MainActor public func extensionMethod()
  public func extensionMethod() {}
}

// CHECK: nonisolated public func libNonisolated()
nonisolated public func libNonisolated() {}

internal struct NoTypecheckInternal {
  static var value: Int { 0 }
  func method() {}
}

internal func noTypecheckUser() -> NoTypecheckInternal { NoTypecheckInternal() }

//--- client.swift

import Lib

nonisolated func useIt() {
  libFunc() // expected-error@:3 {{call to main actor-isolated global function 'libFunc()' in a synchronous nonisolated context}}

  let c = LibClass() // expected-error@:11 {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  c.method() // expected-error@:5 {{call to main actor-isolated instance method 'method()' in a synchronous nonisolated context}}

  let s = LibStruct() // expected-error@:11 {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  s.method() // expected-error@:5 {{call to main actor-isolated instance method 'method()' in a synchronous nonisolated context}}
  s.extensionMethod() // expected-error@:5 {{call to main actor-isolated instance method 'extensionMethod()' in a synchronous nonisolated context}}

  libNonisolated()
}

nonisolated func viaExistential(p: any LibProto) {
  p.requirement() // expected-error@:5 {{call to main actor-isolated instance method 'requirement()' in a synchronous nonisolated context}}
}

@MainActor func viaExistentialMainActor(p: any LibProto) {
  p.requirement()
}

nonisolated func viaGeneric<T: LibProto>(t: T) {
  t.requirement() // expected-error@:5 {{call to main actor-isolated instance method 'requirement()' in a synchronous nonisolated context}}
}

@MainActor func viaGenericMainActor<T: LibProto>(t: T) {
  t.requirement()
}

@MainActor func useItIsolated() {
  libFunc()
  LibClass().method()
  LibStruct().extensionMethod()
}
