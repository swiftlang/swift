// RUN: %target-swift-emit-sil -sil-verify-all -verify %s

class Klass {}

struct E {
    var k = Klass()
}

struct E2 : ~Copyable {
    var k = Klass()
}

var g: () -> () = {}
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

    init(x: ()) { // expected-error {{'self' consumed more than once}}
                  // expected-note@+7{{consumed here}}
                  // expected-note@+7{{consumed again here}}
                  // expected-error@-3{{missing reinitialization of closure capture 'self' after consume}}
                  // expected-note@+5{{consumed here}}
        e = E()
        e2 = E2()
        func capture() {
            let _ = self
            let _ = self.e2
        }
        capture()
    }

    init(y: ()) { // expected-error {{missing reinitialization of closure capture 'self' after consume}}
        e = E()
        e2 = E2()
        func capture() {
            let _ = self // expected-note {{consumed here}}
        }
        capture()
    }

    init(z: ()) {
        e = E()
        e2 = E2()
        func capture() {
            let _ = self // expected-note {{captured here}}
        }
        capture()
        g = capture // expected-error {{escaping local function captures mutating 'self' parameter}}
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
