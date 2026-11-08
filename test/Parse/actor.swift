// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency



actor MyActor1 // expected-cparser-error {{expected '{' in actor}}

actor MyActor2 { // expected-note@:16 {{to match this opening '{'}} expected-astgen-error {{found an unexpected second identifier in actor; is there an accidental break?}} expected-astgen-note {{join the identifiers together}} expected-astgen-note {{join the identifiers together with camel-case}}
    init() {
    }
func hello() { } // expected-cparser-error@+1 {{expected '}' in actor}} expected-astgen-error {{expected '}' to end actor}}
