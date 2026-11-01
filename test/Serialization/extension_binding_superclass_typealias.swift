// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Superclasses named through typealiases in extensions bound later are
// emitted, and clients see them through the module and the module interface.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/Lib.swiftinterface

// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

//--- Lib.swift
public protocol Top {}
open class Base: Top { public init() {} }

open class Derived: A.BaseAlias { public typealias T = Int }
extension Derived.T {}

public enum A {}
extension A { public typealias BaseAlias = Base }

// INTERFACE: @_inheritsConvenienceInitializers open class Derived :
// INTERFACE-NEXT: public typealias T =
// INTERFACE-NEXT: override public init()

//--- Client.swift
import Lib

func takesTop<T: Top>(_: T.Type) {}
func takesBase(_: Base) {}

class Sub: Derived {}

func testConformance() {
  takesTop(Derived.self)
  takesBase(Derived())
  takesBase(Sub())
}
