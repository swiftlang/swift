// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership -o %t %S/Inputs/RetroactiveA.swift
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership -o %t %S/Inputs/RetroactiveB.swift
// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %t %s | %FileCheck %s


import RetroactiveA
import RetroactiveB

struct Z<T: P, U: Hashable, V: Q> { }

extension X: P { } // retroactive
extension Y: Q { } // retroactive

// CHECK: sil hidden @$s20mangling_retroactive5test0yyAA1ZVy12RetroactiveB1XVSiAE1YVAG0D1A1PAAyHCg_AiJ1QAAyHCg1_GF
func test0(_: Z<X, Int, Y>) { }

struct Z2<T: P> {
  struct Inner<V: Q> { }
}

// CHECK: sil hidden @$s20mangling_retroactive5test1yyAA2Z2V5InnerVy12RetroactiveB1XV_AG1YVAI0F1A1PAAyHCg_AkL1QAAyHCg0_GF
func test1(_: Z2<X>.Inner<Y>) { }

extension X: Hashable {
  public static func ==(lhs: X, rhs: X) -> Bool { return true }
  public func hash(into hasher: inout Hasher) { }
}
extension Y: Hashable {
  public static func ==(lhs: Y, rhs: Y) -> Bool { return true }
  public func hash(into hasher: inout Hasher) { }
}

extension Z: Equatable where T: Hashable, V: Equatable {
  static func ==(lhs: Z, rhs: Z) -> Bool { return true }
}

struct RequiresEquatable<T: Equatable> { }

// Conditional requirement involves retroactive conformances.
// CHECK: sil hidden @$s20mangling_retroactive5test2yyAA17RequiresEquatableVyAA1ZVy12RetroactiveB1XVSiAG1YVAI0F1A1PAAyHCg_AkL1QAAyHCg1_GAOSQAISHAAyHC_AKSQAAyHCHCg_GF
func test2(_: RequiresEquatable<Z<X, Int, Y>>) { }
