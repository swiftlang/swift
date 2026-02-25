// RUN: %empty-directory(%t)

// --- SuppressedAssociatedTypesWithDefaults
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypesWithDefaults
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name assoc
// RUN: %FileCheck --check-prefixes=INTERFACE,INTERFACE-NEW %s < %t.swiftinterface

// RUN: %target-swift-frontend -emit-silgen %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -o %t.silgen
// RUN: %FileCheck --check-prefixes=SIL,SIL-NEW %s < %t.silgen

// --- SuppressedAssociatedTypes
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypes
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name assoc
// RUN: %FileCheck --check-prefixes=INTERFACE,INTERFACE-LEGACY %s < %t.swiftinterface

// RUN: %target-swift-frontend -emit-silgen %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypes -o %t.silgen
// RUN: %FileCheck --check-prefixes=SIL,SIL-LEGACY %s < %t.silgen


// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// INTERFACE-LABEL: public protocol P<Element> : ~Copyable {
// INTERFACE:          associatedtype Element : ~Copyable
// INTERFACE:          associatedtype Iterator : ~Copyable
public protocol P<Element>: ~Copyable {
  associatedtype Element: ~Copyable
  associatedtype Iterator: ~Copyable
}

public protocol Ordinary<Element> {
  associatedtype Element
  associatedtype Iterator
}

// For ordinary protocols with associated types, we shouldn't emit any Copyable / ~Copyable...
// INTERFACE-NOT: Copyable
public func ordinary<T: Ordinary>(_ t: T) {}
public struct Thing<T: Ordinary> {}

///////
// Keep in mind that `Rj` and `RJ` mean inverses for associated types!
//////

// INTERFACE: public func ncElement<T>(_ t: T) where T : assoc::P, T.Element : ~Copyable
// SIL: @$s5assoc9ncElementyyxAA1PRz0C0Rj_zlF :
public func ncElement<T: P>(_ t: T) where T.Element: ~Copyable {}

// INTERFACE: public func ncElement_pi<T>(_ t: T) where T : assoc::P, T.Element : ~Copyable
// SIL: @$s5assoc12ncElement_piyyxAA1PRzlF :
@_preInverseGenerics
public func ncElement_pi<T: P>(_ t: T) where T.Element: ~Copyable {}

// INTERFACE: public func ncIter<T>(_ t: T) where T : assoc::P
// SIL-NEW:    @$s5assoc6ncIteryyxAA1PRzlF :
// SIL-LEGACY: @$s5assoc6ncIteryyxAA1PRz7ElementRj_zlF :
public func ncIter<T: P>(_ t: T) where T.Iterator: ~Copyable {}

// INTERFACE: public func ncIter2<T>(_ t: T) where T : assoc::P
// SIL-NEW:    @$s5assoc7ncIter2yyxAA1PRzlF :
// SIL-LEGACY: @$s5assoc7ncIter2yyxAA1PRz7ElementRj_zlF :
public func ncIter2<T: P>(_ t: T) {
  // NOTE: this print is executed in a different test
  print("hello \(t)")
}

// vv Compare ncIter3 with ncIter2 ^^
// To get the same mangling under both versions of the feature,
// one must be specific about the Element being Copyable, since
// it's not assumed to be under the old feature, but it IS under the new one.

// INTERFACE: public func ncIter3<T>(_ t: T) where T : assoc::P
// SIL    @$s5assoc7ncIter3yyxAA1PRzlF :
public func ncIter3<T: P>(_ t: T) where T.Element: Copyable {}

// INTERFACE: public func ncBoth<T>(_ t: T) where T : assoc::P, T.Element : ~Copyable
// SIL: @$s5assoc6ncBothyyxAA1PRz7ElementRj_zlF :
public func ncBoth<T: P>(_ t: T) where T.Iterator: ~Copyable, T.Element: ~Copyable {}

// INTERFACE: public func bothCopyable<T>(_ t: T) where T : assoc::P, T.Iterator : Swift::Copyable
// SIL-NEW:    @$s5assoc12bothCopyableyyxAA1PRzs0C08IteratorRpzlF :
// SIL-LEGACY: @$s5assoc12bothCopyableyyxAA1PRzs0C08IteratorRpz7ElementRj_zlF :
public func bothCopyable<T: P>(_ t: T) where T.Iterator: Copyable {}

// INTERFACE-LABEL: public struct Holder<S> where S : assoc::P, S : ~Copyable, S.Element : ~Copyable {
public struct Holder<S> where S.Element: ~Copyable, S: P & ~Copyable {

// INTERFACE: public func defaultedElement<T>(_ t: T) where T : assoc::P
// SIL-NEW:    @$s5assoc6HolderVAARi_z7ElementRj_zrlE09defaultedC0yyqd__AA1PRd__lF :
// SIL-LEGACY: @$s5assoc6HolderVAARi_z7ElementRj_zrlE09defaultedC0yyqd__AA1PRd__ADRj_d__lF :
  public func defaultedElement<T>(_ t: T) where T: P {}

// INTERFACE: public func ncElement<T>(_ t: T) where T : assoc::P, T.Element : ~Copyable
// SIL: @$s5assoc6HolderVAARi_z7ElementRj_zrlE02ncC0yyqd__AA1PRd__ADRj_d__lF :
  public func ncElement<T>(_ t: T) where T: P, T.Element: ~Copyable {}

// INTERFACE: public func copyableIter<T>(_ t: T) where T : assoc::P, T.Iterator : Swift::Copyable
// SIL-NEW:    @$s5assoc6HolderVAARi_z7ElementRj_zrlE12copyableIteryyqd__AA1PRd__s8Copyable8IteratorRpd__lF :
// SIL-LEGACY: @$s5assoc6HolderVAARi_z7ElementRj_zrlE12copyableIteryyqd__AA1PRd__s8Copyable8IteratorRpd__ADRj_d__lF :
public func copyableIter<T: P>(_ t: T) where T.Iterator: Copyable {}
}

// INTERFACE-NEW: extension assoc::Holder {
// INTERFACE-LEGACY: extension assoc::Holder where S.Element : ~Copyable {
extension Holder {
  // SIL-NEW:    @$s5assoc6HolderV19forceIntoInterface1yyF :
  // SIL-LEGACY: @$s5assoc6HolderVAA7ElementRj_zrlE19forceIntoInterface1yyF :
  public func forceIntoInterface1() {}

  // SIL:        @$s5assoc6HolderV20forceIntoInterface1cyyF :
  public func forceIntoInterface1c() where S.Element: Copyable {}
}

// INTERFACE-NEW: extension assoc::Holder where S.Iterator : Swift::Copyable {
// INTERFACE-LEGACY: extension assoc::Holder where S.Iterator : Swift::Copyable, S.Element : ~Copyable {
extension Holder where S.Iterator: Copyable {
  // SIL-NEW:    @$s5assoc6HolderVAAs8Copyable8IteratorRpzrlE19forceIntoInterface2yyF :
  // SIL-LEGACY: @$s5assoc6HolderVAAs8Copyable8IteratorRpz7ElementRj_zrlE19forceIntoInterface2yyF :
  public func forceIntoInterface2() {}

  // SIL:        @$s5assoc6HolderVAAs8Copyable8IteratorRpzrlE20forceIntoInterface2cyyF :
  public func forceIntoInterface2c() where S.Element: Copyable {}
}

// INTERFACE: extension assoc::Holder where S.Element : ~Copyable {
extension Holder where S.Element: ~Copyable {
  // SIL: @$s5assoc6HolderVAA7ElementRj_zrlE19forceIntoInterface5yyF :
  public func forceIntoInterface5() {}
}

// INTERFACE-NEW: extension assoc::P {
// INTERFACE-LEGACY: extension assoc::P where Self.Element : ~Copyable {
extension P {
  // SIL-NEW:    @$s5assoc1PPAAE19forceIntoInterface3yyF :
  // SIL-LEGACY: @$s5assoc1PPAA7ElementRj_zrlE19forceIntoInterface3yyF :
  public func forceIntoInterface3() {}
  // SIL:        @$s5assoc1PPAAE20forceIntoInterface3cyyF :
  public func forceIntoInterface3c() where Element: Copyable {}
}

// INTERFACE-NEW: extension assoc::P where Self.Iterator : Swift::Copyable {
// INTERFACE-LEGACY: extension assoc::P where Self.Iterator : Swift::Copyable, Self.Element : ~Copyable {
extension P where Iterator: Copyable {
  // SIL-NEW:    @$s5assoc1PPAAs8Copyable8IteratorRpzrlE19forceIntoInterface4yyF :
  // SIL-LEGACY: @$s5assoc1PPAAs8Copyable8IteratorRpz7ElementRj_zrlE19forceIntoInterface4yyF :
  public func forceIntoInterface4() {}

  // SIL:        @$s5assoc1PPAAs8Copyable8IteratorRpzrlE20forceIntoInterface4cyyF :
  public func forceIntoInterface4c() where Element: Copyable {}
}
