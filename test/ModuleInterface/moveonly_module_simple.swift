// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -module-name Hello -emit-module -o %t/Hello.swiftmodule -emit-module-interface-path %t/Hello.swiftinterface %S/Inputs/moveonly_simple.swift -swift-version 5
// RUN: %FileCheck %s < %t/Hello.swiftinterface
// RUN: %target-swift-frontend -emit-sil -verify -I %t %s -swift-version 5

import Hello

// CHECK:      #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:   public enum SwiftCoin : ~Swift.Copyable {

// CHECK:      #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:   @_moveOnly public enum ObjCCoin {

func eat(_ c: consuming SwiftCoin) {}
func eat(_ c: consuming ObjCCoin) {}

public func ensureItIsNonCopyable(_ c: consuming SwiftCoin) { // expected-error {{'c' consumed more than once}}
  eat(c) // expected-note {{consuming use here}}
  eat(c) // expected-note {{consuming use here}}
}

public func ensureItIsNonCopyable(_ c: consuming ObjCCoin) { // expected-error {{'c' consumed more than once}}
  eat(c) // expected-note {{consuming use here}}
  eat(c) // expected-note {{consuming use here}}
}
