// RUN: %target-typecheck-verify-swift

// '@unsafe(always)' requires the AlwaysUnsafeAttribute experimental feature,
// while plain '@unsafe' does not.

@unsafe(always) func alwaysUnsafe() { }
// expected-error@-1{{'unsafe(always)' attribute is only valid when experimental feature AlwaysUnsafeAttribute is enabled}}

@unsafe func unsafe() { }
