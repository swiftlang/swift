// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -module-name Lib \
// RUN:   -strict-memory-safety \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift

// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -sil-verify-all \
// RUN:   -strict-memory-safety \
// RUN:   -module-name Client -I %t \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   %t/Client.swift

// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -sil-verify-all \
// RUN:   -enable-library-evolution \
// RUN:   -strict-memory-safety \
// RUN:   -module-name Lib \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   %t/Lib.swift %t/LibChecks.swift

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

// Optional chains whose bases come from a library built with library evolution.

//--- Lib.swift
@frozen
public struct View: ~Escapable {
  @usableFromInline var p: Int

  @_lifetime(borrow base)
  init(_ base: borrowing Base) { p = 0 }

  @_lifetime(borrow base)
  init(_ base: borrowing FrozenBase) { p = 0 }

  @_lifetime(borrow base)
  init(_ base: borrowing NoncopyableBase) { p = 0 }

  @_lifetime(borrow base)
  init(_ base: borrowing Plain) { p = 0 }
}

public struct Plain {
  public var id: String
  public init(id: String) { self.id = id }

  public var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

@_addressableForDependencies
public struct Base {
  public var id: String
  public init(id: String) { self.id = id }

  public var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

@frozen
@_addressableForDependencies
public struct FrozenBase {
  public var id: String
  public init(id: String) { self.id = id }

  public var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

@_addressableForDependencies
public struct NoncopyableBase: ~Copyable {
  public var id: String
  public init(id: String) { self.id = id }

  public var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

public struct ResilientOuter {
  public var inner: Base?
  public init(inner: Base?) { self.inner = inner }
}

@frozen
public struct FrozenOuter {
  public var inner: Base?
  public init(inner: Base?) { self.inner = inner }
}

@frozen
public struct FrozenPlainWrapper {
  public var inner: Plain?
  public init(inner: Plain?) { self.inner = inner }
}

//--- LibChecks.swift
func f(_ s: View?) {}

@inlinable
public func g(_ s: View?) {}

func sameModuleHop(_ o: ResilientOuter) {
  f(o.inner?.view)
}

@inlinable
public func inlinableBase(_ b: Base?) {
  g(b?.view)
}

@inlinable
public func inlinablePlain(_ p: Plain?) {
  g(p?.view)
}

@inlinable
public func inlinableFrozenHop(_ o: FrozenOuter) {
  g(o.inner?.view)
}

@inlinable
public func inlinableFrozenPlainHop(_ o: FrozenPlainWrapper) {
  g(o.inner?.view)
}

@inlinable
public func inlinableResilientHop(_ o: ResilientOuter) {
  g(o.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

//--- Client.swift
import Lib

func f(_ s: View?) {}

func param(_ b: Base?) {
  f(b?.view)
}

func inoutBase(_ b: inout Base?) {
  f(b?.view)
}

func localVar(_ initial: Base?) {
  var b = initial
  b = initial
  f(b?.view)
}

func localLet(_ initial: Base?) {
  let b = initial
  f(b?.view)
}

func bindToLet(_ b: Base?) {
  let v = b?.view
  f(v)
}

func frozenBase(_ b: FrozenBase?) {
  f(b?.view)
}

func plain(_ p: Plain?) {
  f(p?.view)
}

func plainInout(_ p: inout Plain?) {
  f(p?.view)
}

func plainLocal(_ initial: Plain?) {
  var p = initial
  p = initial
  f(p?.view)
}

func noncopyableBase(_ b: borrowing NoncopyableBase?) {
  f(b?.view)
}

func frozenHop(_ o: FrozenOuter) {
  f(o.inner?.view)
}

func frozenHopInout(_ o: inout FrozenOuter) {
  f(o.inner?.view)
}

func frozenNested(_ o: FrozenOuter?) {
  f(o?.inner?.view)
}

func frozenPlainHop(_ o: FrozenPlainWrapper) {
  f(o.inner?.view)
}

func frozenPlainHopInout(_ o: inout FrozenPlainWrapper) {
  f(o.inner?.view)
}

func resilientHop(_ o: ResilientOuter) {
  f(o.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func resilientHopInout(_ o: inout ResilientOuter) {
  f(o.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func resilientNested(_ o: ResilientOuter?) {
  f(o?.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}
