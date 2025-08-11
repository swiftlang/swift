// RUN: %target-typecheck-verify-swift -verify -target %target-cpu-apple-macosx11.2 -disable-objc-attr-requires-foundation-module
// REQUIRES: OS=macosx

@available(macOS 11.0, *)
class Foo {
    func foo() {
        if #available(macOS 11.1, *) {}
    }
}
