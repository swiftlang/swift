// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 6 -enable-library-evolution \
// RUN:   -enable-upcoming-feature NonisolatedNonsendingByDefault \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// RN: %FileCheck %t/src/A.swift --input-file %t/A.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/A.swiftinterface) -module-name A

// Build the client using module
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

//--- A.swift
public protocol P {
  func doSomething<T>(_ body: () throws -> T) rethrows -> T
}

public struct Test {
}

extension Test: P {
  public func doSomething<E, R>(_ body: () throws(E) -> R) throws(E) -> R { fatalError() }
}

public enum MyError: Error {
}

public struct TestWithConcreteError {
}

extension TestWithConcreteError: P {
  public func doSomething<E, R>(_ body: () throws(E) -> R) throws(E) -> R { fatalError() }
}

//--- Client.swift
import A

// Check that neither regular `rethrows` nor typed throws declared in the current module create witness ambiguity with a typed throw witness of protocol `P` declared in module `A`

protocol Q {
  func doSomething<T>(_ body: () throws -> T) rethrows -> T
}

// protocol witness for Q.doSomething<A>(_:) in conformance Test
// CHECK-LABEL: sil private [transparent] [thunk] @$s1A4TestV6Client1QA2dEP11doSomethingyqd__qd__yKXEKlFTW
// CHECK: [[WITNESS:%.*]] = function_ref @$s1A4TestV6ClientE11doSomethingyxxyKXEKlF
// CHECK: try_apply [[WITNESS]]<τ_0_0>({{.*}})
// CHECK: } // end sil function '$s1A4TestV6Client1QA2dEP11doSomethingyqd__qd__yKXEKlFTW'
extension Test: Q {
  func doSomething<T>(_ body: () throws -> T) rethrows -> T { fatalError() }
}

protocol W {
  func doSomething<T, E>(_ body: () throws -> T) throws(E) -> T
}

// protocol witness for W.doSomething<A, B>(_:) in conformance Test
// CHECK-LABEL: sil private [transparent] [thunk] @$s1A4TestV6Client1WA2dEP11doSomethingyqd__qd__yKXEqd_0_YKs5ErrorRd_0_r0_lFTW
// CHECK: [[WITNESS:%.*]] = function_ref @$s1A4TestV6ClientE11doSomethingyxxyKXEq_YKs5ErrorR_r0_lF
// CHECK: try_apply [[WITNESS]]<τ_0_0, τ_0_1>({{.*}})
// CHECK: } // end sil function '$s1A4TestV6Client1WA2dEP11doSomethingyqd__qd__yKXEqd_0_YKs5ErrorRd_0_r0_lFTW'
extension Test: W {
  func doSomething<T, E>(_ body: () throws -> T) throws(E) -> T { fatalError() }
}

protocol U {
  func doSomething<T>(_ body: () throws -> T) throws(MyError) -> T  
}

// protocol witness for U.doSomething<A>(_:) in conformance TestWithConcreteError
// CHECK-LABEL: sil private [transparent] [thunk] @$s1A21TestWithConcreteErrorV6Client1UA2dEP11doSomethingyqd__qd__yKXEAA02MyD0OYKlFTW
// CHECK: [[WITNESS:%.*]] = function_ref @$s1A21TestWithConcreteErrorV6ClientE11doSomethingyxxyKXEAA02MyD0OYKlF
// CHECK: try_apply [[WITNESS]]<τ_0_0>({{.*}})
// CHECK: } // end sil function '$s1A21TestWithConcreteErrorV6Client1UA2dEP11doSomethingyqd__qd__yKXEAA02MyD0OYKlFTW'
extension TestWithConcreteError: U {
  func doSomething<T>(_ body: () throws -> T) throws(MyError) -> T { fatalError() }
}
