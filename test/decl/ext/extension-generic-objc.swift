// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

// Tests for correct detection of the "objc_in_generic_extension" error,
// declared in {Swift Source}/include/swift/AST/DiagnosticsSema.def

// Test "0 levels" deep
class A<T> : NSObject {
    init(a: ()) {
        super.init()
    }
}
extension A {
    // This should throw an error
    @objc func a1() {} // expected-error{{@objc is not supported within extensions of generic classes or classes that inherit from generic classes}}
    // This should *not* throw an error
    func a2() {}
}

// Test "1 level" deep
class B : A<Int> {
    init(b: ()) {
        super.init(a: ())
    }
}
extension B {
    // This should throw an error
    @objc func b1() {} // expected-error{{@objc is not supported within extensions of generic classes or classes that inherit from generic classes}}
    // This should *not* throw an error
    func b2() {}
}

// Test "many levels" deep
class C : B {}
class D : C {
    init(d: ()) {
        super.init(b: ())
    }
}
extension D {
    // This should throw an error
    @objc func d1() {} // expected-error{{@objc is not supported within extensions of generic classes or classes that inherit from generic classes}}
    // This should *not* throw an error
    func d2() {}
}

class Outer<T> {
    class Inner {}
}

extension Outer.Inner {
    @objc func outerInner1() {}
    // expected-error@-1{{@objc is not supported within extensions of generic classes or classes that inherit from generic classes}}
    func outerInner2() {}
}
