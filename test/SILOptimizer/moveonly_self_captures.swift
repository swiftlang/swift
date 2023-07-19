// RUN: %target-swift-emit-sil -sil-verify-all -verify %s

class Klass {}

struct E {
    var k = Klass()
}

struct E2 : ~Copyable {
    var k = Klass()
}

struct Test : ~Copyable {
    var e: E
    var e2: E2

    // Test that we capture inits by address.
    init() {
        e = E()
        e2 = E2()
        func capture() {
            let _ = self.e
        }
        capture()
    }

    init(x: ()) {
        e = E()
        e2 = E2()
        func capture() {
            let _ = self // expected-error {{copy of noncopyable typed value}}
            let _ = self.e2 // expected-error {{copy of noncopyable typed value}}
        }
        capture()
    }

    func captureByLocalFunction() {
        func capture() {
            let _ = self.e
        }
        capture()
    }

    func captureByLocalFunction2() { // expected-error {{noncopyable 'self' cannot be consumed when captured by an escaping closure}}
        func capture() {
            let _ = self.e2 // expected-note {{consumed here}}
        }
        capture()
    }

    func captureByLocalFunction3() { // expected-error {{noncopyable 'self' cannot be consumed when captured by an escaping closure}}
        func capture() {
            let _ = self // expected-note {{consumed here}}
        }
        capture()
    }

    func captureByLocalLet() { // expected-error {{'self' cannot be captured by an escaping closure since it is a borrowed parameter}}
        let f = { // expected-note {{capturing 'self' here}}
            let _ = self.e
        }
        
        f()
    }

    func captureByLocalVar() { // expected-error {{'self' cannot be captured by an escaping closure since it is a borrowed parameter}}
        var f = {}
        f = { // expected-note {{closure capturing 'self' here}}
            let _ = self.e
        }
        f()
    }

    func captureByNonEscapingClosure() {
        func useClosure(_ f: () -> ()) {}
        useClosure {
            let _ = self.e
        }
    }

    func captureByNonEscapingClosure2() { // expected-error {{'self' cannot be consumed when captured by an escaping closure}}
        func useClosure(_ f: () -> ()) {}
        useClosure {
            let _ = self // expected-note {{consumed here}}
        }
    }
}


