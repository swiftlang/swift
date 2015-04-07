// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/AlexDenisov (Alexey Denisov)

func i(c: () -> ()) {
}
class a {
    var _ = i() { // expected-error {{property declaration does not bind any variables}}
    }
}
