// RUN: %target-typecheck-verify-swift -disable-availability-checking

// REQUIRES: concurrency



actor MyActor1 // expected-error {{expected '{' in actor}}

actor MyActor2 { // expected-note@:16 {{to match this opening '{'}}
    init() {
    }
func hello() { } // expected-error@+1 {{expected '}' in actor}}
