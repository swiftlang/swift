// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -sil-verify-all | %FileCheck %s

// REQUIRES: swift_in_compiler

public protocol OptionalType<Wrapped> {
  associatedtype Wrapped

  static func some(_ wrapped: Wrapped) -> Self
  static func somePair(_ wrapped1: Wrapped, _ wrapped2: Wrapped) -> Self
  static func esc(_ e: E) -> Self
}

public struct E {}

public enum FakeOptional<Wrapped : ~Escapable & ~Copyable> : ~Escapable & ~Copyable {
  case none
  case some(Wrapped)
  case somePair(Wrapped, Wrapped)
  case esc(E)
}

extension FakeOptional: Copyable where Wrapped: Copyable & ~Escapable {}

extension FakeOptional: Escapable where Wrapped: Escapable & ~Copyable {}

extension FakeOptional : OptionalType {}

// CHECK-LABEL: sil shared [transparent] @$s25lifetime_dependence_enums12FakeOptionalO4someyACyxGxcAEmlF : $@convention(method) <Wrapped where Wrapped : ~Copyable, Wrapped : ~Escapable> (@in Wrapped, @thin FakeOptional<Wrapped>.Type) -> @lifetime(copy 0) @out FakeOptional<Wrapped> {

// CHECK-LABEL: sil shared [transparent] @$s25lifetime_dependence_enums12FakeOptionalO8somePairyACyxGx_xtcAEmlF : $@convention(method) <Wrapped where Wrapped : ~Copyable, Wrapped : ~Escapable> (@in Wrapped, @in Wrapped, @thin FakeOptional<Wrapped>.Type) -> @lifetime(copy 0, copy 1) @out FakeOptional<Wrapped> {

// CHECK-LABEL: sil shared [transparent] @$s25lifetime_dependence_enums12FakeOptionalO3escyACyxGAA1EVcAEmlF : $@convention(method) <Wrapped where Wrapped : ~Copyable, Wrapped : ~Escapable> (E, @thin FakeOptional<Wrapped>.Type) -> @out FakeOptional<Wrapped> {

