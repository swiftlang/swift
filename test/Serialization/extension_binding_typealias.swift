// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Conformances to protocols from extensions bound later are emitted, and
// clients see them through the module and the module interface.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib
// RUN: %target-swift-frontend -emit-ir %t/Lib.swift -module-name Lib \
// RUN:   | %FileCheck %s --check-prefix=IR

// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

//--- Lib.swift
public struct S: A.P { public typealias T = Int }
extension S.T {}

public struct Implied: A.P2 { public typealias T = Int }
extension Implied.T {}

open class Base: A.P {}
open class Derived: Base { public typealias T = Int }
extension Derived.T {}

public struct Outer: A.P { public struct Nested { public typealias T = Int } }
extension Outer.Nested.T {}

open class Base2 {}
open class Derived2: Base2, A.P { public typealias T = Int }
extension Derived2.T {}

public protocol Top {}
public enum A {}
extension A { public protocol P {} }
extension A { public protocol P2: Top {} }

// IR-DAG: @"$s3Lib1SVAA1AO1PAAWP" =
// IR-DAG: @"$s3Lib7ImpliedVAA1AO2P2AAWP" =
// IR-DAG: @"$s3Lib7ImpliedVAA3TopAAWP" =
// IR-DAG: @"$s3Lib4BaseCAA1AO1PAAWP" =
// IR-DAG: @"$s3Lib5OuterVAA1AO1PAAWP" =
// IR-DAG: @"$s3Lib8Derived2CAA1AO1PAAWP" =

//--- Client.swift
import Lib

func takesP<T: A.P>(_: T.Type) {} // expected-note {{where 'T' = 'Outer.Nested'}} expected-note {{where 'T' = 'Base2'}}
func takesP2<T: A.P2>(_: T.Type) {}
func takesTop<T: Top>(_: T.Type) {}

func testConformance() {
  takesP(S.self)
  takesP2(Implied.self)
  takesTop(Implied.self)
  takesP(Base.self)
  takesP(Derived.self)
  takesP(Outer.self)
  takesP(Derived2.self)
}

func testNonConformance() {
  takesP(Outer.Nested.self) // expected-error {{global function 'takesP' requires that 'Outer.Nested' conform to 'A.P'}}
  takesP(Base2.self) // expected-error {{global function 'takesP' requires that 'Base2' conform to 'A.P'}}
}
