// Ensure that the `unnecessary check` availability warning is emitted when unnecessary due to
// scope's explicit annotation

// RUN: %target-typecheck-verify-swift -verify -target %target-cpu-apple-macosx11.2 -verify-additional-prefix noerror-
// RUN: %target-typecheck-verify-swift -verify -target %target-cpu-apple-macosx11.2 -Werror UselessAvailabilityCheck -verify-additional-prefix werror-
// REQUIRES: OS=macosx

@available(macOS 11.1, *)
class Foo {
    // expected-note@-1 {{enclosing scope here}}
    func foo() {
        // expected-noerror-warning@+2 {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
        // expected-werror-error@+1 {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
        if #available(macOS 11.0, *) {}
    }
}
