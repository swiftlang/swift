// RUN: %target-swift-frontend -emit-sil -verify %s

class Class {}
//struct Class : ~Copyable {}

func consume(_: consuming Class) {}
func nonescapingClosure(_ body: () -> ()) {
    body()
}

func testNonescapingCaptureConsuming(x: consuming Class) { // expected-error{{}}
    nonescapingClosure { consume(x) } // expected-note{{consumed here}}
}

// TODO: `defer` should be allowed to consume local bindings
func testDeferCaptureConsuming(x: consuming Class) { // expected-error{{}}
    defer { consume(x) } // expected-note{{consumed here}}
    do {}
}

func testLocalFunctionCaptureConsuming(x: consuming Class) {
    func local() {
        consume(x) // expected-error{{cannot be consumed when captured by an escaping closure}}
    }
}

func testNonescapingCaptureBorrowing(x: borrowing Class) {
    nonescapingClosure { consume(x) } // expected-error{{'x' is borrowed by this closure, so it cannot be consumed here; you must 'copy x' before consumption}}
}

func testDeferCaptureBorrowing(x: borrowing Class) {
    defer { consume(x) } // expected-error{{'x' is borrowed by this deferred block, so it cannot be consumed here; you must 'copy x' before consumption}}
    do {}
}

func testLocalFunctionCaptureBorrowing(x: borrowing Class) {
    func local() {
        consume(x) // expected-error{{'x' is borrowed by this closure, so it cannot be consumed here; you must 'copy x' before consumption}}
    }
}
