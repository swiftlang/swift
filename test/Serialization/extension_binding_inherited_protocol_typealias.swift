// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Protocols that inherit from protocols in extensions bound later are
// emitted with those protocols, and clients see them through the module and
// the module interface.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib

// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

//--- Lib.swift
public protocol Refined: A.P {}
public struct S: Refined { public typealias T = Int; public init() {} }
extension S.T {}

public enum A {}
extension A { public protocol P {} }

//--- Client.swift
import Lib

func takesP(_: any A.P) {}
func generic<T: Refined>(_ t: T) { takesP(t) }
struct Mine: Refined {}

func testConformance() {
  takesP(S())
  generic(S())
  takesP(Mine())
}
