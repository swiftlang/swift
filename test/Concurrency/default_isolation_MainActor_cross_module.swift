// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 5 \
// RUN:   -default-isolation MainActor \
// RUN:   -enable-library-evolution \
// RUN:     -emit-module-path %t/A.swiftmodule \
// RUN:     -emit-module-interface-path %t/A.swiftinterface

// RUN: %FileCheck %t/src/A.swift < %t/A.swiftinterface

// Build the client using module
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t -primary-file %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t -primary-file %t/src/Client.swift

// REQUIRES: concurrency

//--- A.swift

// CHECK: @_Concurrency.MainActor @preconcurrency public protocol P
public protocol P {
}

// CHECK: nonisolated public protocol Q : Swift.Sendable
nonisolated public protocol Q: Sendable {
}

// CHECK: @_Concurrency.MainActor @preconcurrency public struct S : A.P {
public struct S: P {
  // CHECK:   @_Concurrency.MainActor @preconcurrency public func f()
  public func f() {}

  // CHECK: @_Concurrency.MainActor @preconcurrency public struct Inner {
  public struct Inner {
    // CHECK: @_Concurrency.MainActor @preconcurrency public init()
    public init() {}
  }
  // CHECK: }
}
// CHECK: }

// CHECK: public struct R : A.Q {
public struct R: Q {
  // CHECK: public struct Inner {
  public struct Inner {
    // CHECK: public init()
    public init() {}
  }
  // CHECK: }

  // CHECK: @_Concurrency.MainActor @preconcurrency public struct InnerIsolated : A.P {
  // CHECK: }
  public struct InnerIsolated: P {}
}
// CHECK: }

// CHECK: @_Concurrency.MainActor @preconcurrency public func testGlobal()
public func testGlobal() {}

// CHECK: @_Concurrency.MainActor @preconcurrency public class C {
public class C {
  // CHECK: @_Concurrency.MainActor @preconcurrency public init()
  public init() {}

  // CHECK: @_Concurrency.MainActor @preconcurrency public static var value: Swift.Int
  public static var value = 42

  // CHECK: {{(@objc )?}} @_Concurrency.MainActor deinit
}
// CHECK: }

// CHECK: @_Concurrency.MainActor @preconcurrency open class IsolatedDeinitTest {
open class IsolatedDeinitTest {
  // CHECK:   {{(@objc )?}} isolated deinit
  isolated deinit {}
}
// CHECK: }


//--- Client.swift
import A

final class IsolatedDeinitChild: IsolatedDeinitTest {} // Ok

nonisolated func testIsolation() {
  testGlobal() // expected-warning {{call to main actor-isolated global function 'testGlobal()' in a synchronous nonisolated context}}

  func test(s: S) {
    s.f() // expected-warning {{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  }

  _ = S.Inner() // expected-warning {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  _ = R.Inner() // Ok
  
  _ = C() // expected-warning {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  _ = C.value // expected-warning {{main actor-isolated class property 'value' can not be referenced from a nonisolated context}}
}
