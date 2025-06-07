// RUN: %target-typecheck-verify-swift

// rdar://105080067 - This isn't currently allowed but make sure we don't
// emit a spurious "cannot reference invalid declaration" error.
for b in [true] where if b { true } else { false } {}
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
