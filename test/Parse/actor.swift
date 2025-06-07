// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency



actor MyActor1 // expected-error {{expected '{' in actor}}

actor MyActor2 { // expected-note@:16 {{to match this opening '{'}}
    init() {
    }
func hello() { } // expected-error@+1 {{expected '}' in actor}}
