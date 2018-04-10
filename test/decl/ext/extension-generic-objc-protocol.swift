// RUN: %target-swift-frontend -typecheck -verify %s

// REQUIRES: objc_interop

import Foundation

@objc protocol P {}

public class C1<T> {}
extension C1: P {}
// expected-error@-1 {{conformance of generic class 'C1<T>' to @objc protocol 'P' cannot be in an extension}}

public class C2<T> {}
public class C3 : C2<Int> {}
extension C3: P {}
// expected-error@-1 {{conformance of subclass of a generic class 'C3' to @objc protocol 'P' cannot be in an extension}}

class Outer<T> {
    class Inner {}
    class Inner2 {}
}

extension Outer.Inner: P {}
// expected-error@-1 {{conformance of class from generic context 'Outer<T>.Inner' to @objc protocol 'P' cannot be in an extension}}

class SubInner: Outer<Int>.Inner2 {}

extension SubInner: P {}
// expected-error@-1 {{conformance of subclass of a class from generic context 'SubInner' to @objc protocol 'P' cannot be in an extension}}
