// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -module-name _A -swift-version 5 \
// RUN:   -strict-concurrency=complete \
// RUN:   -package-name A \
// RUN:   -enable-library-evolution \
// RUN:     -emit-module-path %t/A.swiftmodule \
// RUN:     -emit-module-interface-path %t/A.swiftinterface \
// RUN:     -emit-package-module-interface-path %t/A.package.swiftinterface \
// RUN:     -emit-private-module-interface-path %t/A.private.swiftinterface \
// RUN:   -verify

// RUN: %target-swift-typecheck-module-from-interface(%t/A.package.swiftinterface) -module-name _A
// RUN: %target-swift-typecheck-module-from-interface(%t/A.private.swiftinterface) -module-name _A

// RUN: %FileCheck %s < %t/A.package.swiftinterface

// REQUIRES: concurrency

// CHECK: package struct S : Swift::Sendable {
package struct S {
  package let x: Int = 42
}

private class NonSendable {
}

@usableFromInline
protocol P: Sendable {
}

// CHECK: final package class A : _A::P, Swift::Sendable {
package final class A: P {
  let x: Int = 42
}

// CHECK: package struct NS {
// CHECK: }
package struct NS {
  private var v: NonSendable
}

// CHECK: package enum Unavailable : Swift::Equatable {
package enum Unavailable: Equatable {
  case test
}

@available(*, unavailable)
extension Unavailable: Sendable {
}

// CHECK: @usableFromInline
// CHECK: internal struct InternalTest {
// CHECK: }
@usableFromInline
struct InternalTest {
  var x: Int = 42
}

// CHECK: package struct ConditionalTest<T> where T : AnyObject {
package struct ConditionalTest<T: AnyObject> {
  weak var test: T?
}

extension ConditionalTest: Sendable where T: Sendable {
}

@available(macOS 15.0, *)
@_originallyDefinedIn(module: "A", macOS 15.0)
public struct Test1: Sendable {
  // CHECK: package enum Storage : Swift::Equatable, Swift::Sendable {
  package enum Storage: Equatable {
    case empty
  }

  package var storage: Storage
}

@available(macOS 15.0, *)
@_originallyDefinedIn(module: "A", macOS 15.0)
public struct Test2: Sendable {
  // CHECK: package struct Storage : Swift::Equatable {
  package struct Storage: Equatable {  // expected-note {{consider making struct 'Storage' conform to the 'Sendable' protocol}}
    // CHECK-NOT: private var ns: NS
    private var ns: NS = NS()
  }

  // CHECK: package class NS : Swift::Equatable {
  package class NS : Equatable {
    package static func ==(_: NS, _: NS) -> Bool { false }
  }

  package var storage: Storage
  // expected-warning@-1 {{stored property 'storage' of 'Sendable'-conforming struct 'Test2' has non-Sendable type 'Test2.Storage'}}
}
