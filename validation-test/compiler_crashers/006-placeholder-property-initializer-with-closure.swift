// RUN: %swift -emit-ir -verify %s
// Test case submitted to project by https://github.com/AlexDenisov (Alexey Denisov)

func i(c: () -> ()) {
}

class a {
    var _ = i() { // expected-error{{property declaration does not bind any variables}}
    }
}
