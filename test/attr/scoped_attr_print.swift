// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -print-ast -enable-experimental-feature ScopeRestrictions %t/printed.swift > %t/reprinted.swift
// RUN: %diff -u %t/printed.swift %t/reprinted.swift

// REQUIRES: swift_feature_ScopeRestrictions

// This ensures @_scoped is printed reasonably, and reparses to itself.
// TODO: throw this test away when we emit @_scoped into interfaces! It is overly coupled to how -print-ast works.

//--- printed.swift

public enum S {
}

public enum Pair {
}
public func named(a: Int, b: @_scoped(a) S) {
}
public func accessScope(a: inout Int, b: @_scoped(&a) S) {
}
public func immortal(b: @_scoped(immortal) S) {
}
public func labelled(a: Int, b: Int, p: @_scoped(left: a, right: b) Pair) {
}
public func labelledAccessScopes(a: inout Int, b: inout Int, p: @_scoped(left: &a, right: &b) Pair) {
}
public func escapedKeyword(default d: Int, b: @_scoped(`default`) S) {
}
public func keywordLabel(a: Int, p: @_scoped(`default`: a) Pair) {
}
public func selfLabel(a: Int, p: @_scoped(`self`: a) Pair) {
}
public func escapedImmortal(_ immortal: Int, b: @_scoped(`immortal`) S) {
}
public func escapedImmortalAccess(_ immortal: inout Int, b: @_scoped(&`immortal`) S) {
}
public func labelledImmortal(a: inout Int, p: @_scoped(`self`: immortal, right: &a) Pair) {
}

extension S {
  public func selfKeyword(b: @_scoped(self) S) {
  }
  public func selfAccessScope(b: @_scoped(&self) S) {
  }
  public func escapedSelf(_ self: Int, b: @_scoped(`self`) S) {
  }
  public func escapedSelfAccess(_ self: inout Int, b: @_scoped(&`self`) S) {
  }
}
