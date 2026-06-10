// Ensure that the `unnecessary check` availability warning is emitted when unnecessary due to
// scope's explicit annotation

// RUN: %target-typecheck-verify-swift -verify -target %target-cpu-apple-macosx11.2
// REQUIRES: OS=macosx

@available(macOS 11.1, *)
class Foo {
    // expected-note@-1 {{enclosing scope here}}
    func foo() {
        // expected-warning@+1 {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}{{group-name=UselessAvailabilityCheck}}
        if #available(macOS 11.0, *) {}
    }
}
