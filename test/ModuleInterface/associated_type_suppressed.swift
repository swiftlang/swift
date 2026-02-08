// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name assoc -enable-experimental-feature SuppressedAssociatedTypesWithDefaults
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name assoc
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: swift_feature_SuppressedAssociatedTypes
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

// CHECK: public func ncElement<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable, T.Iterator : ~Copyable
public func ncElement<T: P>(_ t: T) where T.Element: ~Copyable {}

// CHECK: public func ncIter<T>(_ t: T) where T : assoc.P, T.Iterator : ~Copyable
public func ncIter<T: P>(_ t: T) where T.Iterator: ~Copyable {}
// CHECK: public func ncIter2<T>(_ t: T) where T : assoc.P, T.Iterator : ~Copyable
public func ncIter2<T: P>(_ t: T) {}

// CHECK: public func ncBoth<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable, T.Iterator : ~Copyable
public func ncBoth<T: P>(_ t: T) where T.Iterator: ~Copyable, T.Element: ~Copyable {}

// CHECK: public func bothCopyable<T>(_ t: T) where T : assoc.P, T.Iterator : Swift.Copyable
public func bothCopyable<T: P>(_ t: T) where T.Iterator: Copyable {}

// CHECK-LABEL: public struct Holder<S> where S : assoc.P, S : ~Copyable, S.Element : ~Copyable, S.Iterator : ~Copyable {
public struct Holder<S> where S.Element: ~Copyable, S: P & ~Copyable {
// CHECK: public func defaultedElement<T>(_ t: T) where T : assoc.P, T.Iterator : ~Copyable
  public func defaultedElement<T>(_ t: T) where T: P {}
// CHECK: public func ncElement<T>(_ t: T) where T : assoc.P, T.Element : ~Copyable, T.Iterator : ~Copyable
  public func ncElement<T>(_ t: T) where T: P, T.Element: ~Copyable {}
// CHECK: public func copyableIter<T>(_ t: T) where T : assoc.P, T.Iterator : Swift.Copyable
  public func copyableIter<T: P>(_ t: T) where T.Iterator: Copyable {}
}

// CHECK: extension assoc.Holder where S.Iterator : ~Copyable {
extension Holder {
  public func forceIntoInterface1() {}
}

// CHECK: extension assoc.Holder where S.Iterator : Swift.Copyable {
extension Holder where S.Iterator: Copyable {
  public func forceIntoInterface2() {}
}

// CHECK: extension assoc.P where Self.Iterator : ~Copyable {
extension P {
  public func forceIntoInterface3() {}
}

// CHECK: extension assoc.P where Self.Iterator : Swift.Copyable {
extension P where Iterator: Copyable {
  public func forceIntoInterface4() {}
}
