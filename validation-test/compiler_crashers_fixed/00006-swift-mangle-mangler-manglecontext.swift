// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/AlexDenisov (Alexey Denisov)

func i(c: () -> ()) {
}
class a {
    var _ = i() { // expected-error {{property declaration does not bind any variables}}
    }
}
