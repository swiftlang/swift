// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypesWithDefaults
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name assoc
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -emit-silgen %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -o %t.silgen
// RUN: %FileCheck --check-prefix=SIL %s < %t.silgen

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

// CHECK-LABEL: public protocol P<Element> : ~Copyable {
// CHECK:          associatedtype Element : ~Copyable
// CHECK:          associatedtype Iterator : ~Copyable
public protocol P<Element>: ~Copyable {
  associatedtype Element: ~Copyable
  associatedtype Iterator: ~Copyable
}

public protocol Ordinary<Element> {
  associatedtype Element
  associatedtype Iterator
}

// For ordinary protocols with associated types, we shouldn't emit any Copyable / ~Copyable...
// CHECK-NOT: Copyable
public func ordinary<T: Ordinary>(_ t: T) {}
public struct Thing<T: Ordinary> {}

///////
// Keep in mind that `Rj` and `RJ` mean inverses for associated types!
//////

// CHECK: public func ncElement<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable
// SIL: @$s5assoc9ncElementyyxAA1PRz0C0Rj_zlF :
public func ncElement<T: P>(_ t: T) where T.Element: ~Copyable {}

// CHECK: public func ncElement_pi<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable
// SIL: @$s5assoc12ncElement_piyyxAA1PRzlF :
@_preInverseGenerics
public func ncElement_pi<T: P>(_ t: T) where T.Element: ~Copyable {}

// CHECK: public func ncIter<T>(_ t: T) where T : assoc.P
// SIL: @$s5assoc6ncIteryyxAA1PRzlF :
public func ncIter<T: P>(_ t: T) where T.Iterator: ~Copyable {}

// CHECK: public func ncIter2<T>(_ t: T) where T : assoc.P
// SIL: @$s5assoc7ncIter2yyxAA1PRzlF :
public func ncIter2<T: P>(_ t: T) {}

// CHECK: public func ncBoth<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable
// SIL: @$s5assoc6ncBothyyxAA1PRz7ElementRj_zlF :
public func ncBoth<T: P>(_ t: T) where T.Iterator: ~Copyable, T.Element: ~Copyable {}

// CHECK: public func bothCopyable<T>(_ t: T) where T : assoc.P, T.Iterator : Swift.Copyable
// SIL: @$s5assoc12bothCopyableyyxAA1PRzs0C08IteratorRpzlF :
public func bothCopyable<T: P>(_ t: T) where T.Iterator: Copyable {}

// CHECK-LABEL: public struct Holder<S> where S : assoc.P, S : ~Copyable, S.Element : ~Copyable {
public struct Holder<S> where S.Element: ~Copyable, S: P & ~Copyable {

// CHECK: public func defaultedElement<T>(_ t: T) where T : assoc.P
// SIL: @$s5assoc6HolderVAARi_z7ElementRj_zrlE09defaultedC0yyqd__AA1PRd__lF :
  public func defaultedElement<T>(_ t: T) where T: P {}

// CHECK: public func ncElement<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable
// SIL: @$s5assoc6HolderVAARi_z7ElementRj_zrlE02ncC0yyqd__AA1PRd__ADRj_d__lF :
  public func ncElement<T>(_ t: T) where T: P, T.Element: ~Copyable {}

// CHECK: public func copyableIter<T>(_ t: T) where T : assoc.P, T.Iterator : Swift.Copyable
// SIL: @$s5assoc6HolderVAARi_z7ElementRj_zrlE12copyableIteryyqd__AA1PRd__s8Copyable8IteratorRpd__lF :
public func copyableIter<T: P>(_ t: T) where T.Iterator: Copyable {}
}

// CHECK: extension assoc.Holder {
extension Holder {
  // SIL: @$s5assoc6HolderV19forceIntoInterface1yyF :
  public func forceIntoInterface1() {}
}

// CHECK: extension assoc.Holder where S.Iterator : Swift.Copyable {
extension Holder where S.Iterator: Copyable {
  // SIL: @$s5assoc6HolderVAAs8Copyable8IteratorRpzrlE19forceIntoInterface2yyF :
  public func forceIntoInterface2() {}
}

// CHECK: extension assoc.Holder where S.Element : ~Copyable {
extension Holder where S.Element: ~Copyable {
  // SIL: @$s5assoc6HolderVAA7ElementRj_zrlE19forceIntoInterface5yyF :
  public func forceIntoInterface5() {}
}

// CHECK: extension assoc.P {
extension P {
  // SIL: @$s5assoc1PPAAE19forceIntoInterface3yyF :
  public func forceIntoInterface3() {}
}

// CHECK: extension assoc.P where Self.Iterator : Swift.Copyable {
extension P where Iterator: Copyable {
  // SIL: @$s5assoc1PPAAs8Copyable8IteratorRpzrlE19forceIntoInterface4yyF
  public func forceIntoInterface4() {}
}
