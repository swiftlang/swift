// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-generic-objc-protocol.h -typecheck -verify %s

// REQUIRES: objc_interop

import Foundation

@objc protocol P {}
@objc protocol Q {}
@objc protocol R {}

public class C1<T> {}
extension C1: P {}
// expected-error@-1 {{conformance of generic class 'C1<T>' to '@objc' protocol 'P' cannot be in an extension}}

public class C2<T> {}
public class C3 : C2<Int> {}
extension C3: P {}

class Outer<T> {
    class Inner {}
    class Inner2 {}
}

extension Outer.Inner: P {}
// expected-error@-1 {{conformance of class from generic context 'Outer<T>.Inner' to '@objc' protocol 'P' cannot be in an extension}}

class SubInner: Outer<Int>.Inner2 {}

extension SubInner: P {}

// Lightweight generic ObjC classes can still be extended to conform.

extension OBJCGeneric: OBJCProtocol1 {}
extension OBJCGeneric: P {}
extension OBJCGenericSubclass: OBJCProtocol2 {}
extension OBJCGenericSubclass: Q {}
extension OBJCNongenericSubclass: OBJCProtocol3 {}
extension OBJCNongenericSubclass: R {}

public class SwiftSubclassOfObjCGeneric: OBJCGeneric<AnyObject> {}

extension SwiftSubclassOfObjCGeneric: OBJCProtocol2 {}
extension SwiftSubclassOfObjCGeneric: Q {}

public class SwiftGenericSubclassOfObjCGeneric<T: AnyObject>
  : OBJCGeneric<AnyObject>
{}

extension SwiftGenericSubclassOfObjCGeneric: OBJCProtocol2 {} // expected-error {{cannot be in an extension}}
extension SwiftGenericSubclassOfObjCGeneric: Q {} // expected-error {{cannot be in an extension}}

public class SwiftNongenericSubclassOfGenericSubclassOfObjCGeneric
  : SwiftGenericSubclassOfObjCGeneric<AnyObject>
{}

extension SwiftNongenericSubclassOfGenericSubclassOfObjCGeneric: OBJCProtocol3 {}
extension SwiftNongenericSubclassOfGenericSubclassOfObjCGeneric: R {}

