// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip -enable-experimental-feature TrailingComma

func testTupleAndArgumentsWithTrailingComma() {

    let _ = (a: 1, b: 2, c: 3,)

    func foo(a: Int = 0, b: Int = 0, c: Int = 0,) -> Int {
        return a + b + c
    }

    let _ = foo(a: 1, b: 2, c: 3,)

}

func testConditionsWithTrailingComma() {

    func f(_ block: (Bool) -> Bool) -> Bool { block(true) }

    if true, (f { $0 }), { true }(), { a in a == 1 }(1), { print("if") } else { print("else") }

    if true, { { { } } } // expected-error{{closure expression is unused}} expected-note{{did you mean to use a 'do' statement?}}

    while true, (f { $0 }), { true }(), { a in a == 1 }(1), { print("while") }

    guard true, (f { $0 }), { true }(), { a in a == 1 }(1), else { return }

    guard else { return } // expected-error{{missing condition in 'guard' statement}}

}
