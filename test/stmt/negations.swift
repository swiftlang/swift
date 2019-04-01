// RUN: %target-swift-frontend -typecheck -verify %s

ifn't false {
    print("x")
}

ifn't true {
    print("not executed") // expected-warning {{will never be executed}}
}

ifn't let x = Optional.some(1) { // expected-error {{unsupported}}
}

whilen't false {
    break
}

whilen't true {
    break // expected-warning {{will never be executed}}
}

whilen't let x = Optional.some(1) { // expected-error {{unsupported}}
}

func aFunction() {
    let x: Int
    don't {
        x = 1
    }
    print(x) // expected-error {{uninitialized}}
}

func willInfinitelyRecurse() { // expected-error {{all paths through}}
    don't {
        return
    }
    willInfinitelyRecurse()
}

func containsGuardnt() {
    guardn't true else { return } // expected-warning {{will never be executed}}
    guardn't let x = y else { return } // expected-error {{unsupported}}
}