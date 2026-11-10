// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency



actor MyActor1 // expected-cparser-error {{expected '{' in actor}} expected-astgen-error {{expected member block in actor}}

actor MyActor2 { // expected-note@:16 {{to match this opening '{'}}
    init() {
    }
func hello() { } // expected-cparser-error@+1 {{expected '}' in actor}} expected-astgen-error {{expected '}' to end actor}}
