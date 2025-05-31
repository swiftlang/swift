// RUN: %target-typecheck-verify-swift -enable-experimental-feature InlineArrayTypeSugar -target %target-cpu-apple-macosx15.0

// REQUIRES: swift_feature_InlineArrayTypeSugar
// REQUIRES: OS=macosx

func foo(x: InlineArray<3, Int>) {}
// expected-error@-1 {{'InlineArray' is only available in}}
// expected-note@-2 {{add '@available' attribute to enclosing global function}}

func bar(x: [3 of Int]) {}
// expected-error@-1 {{'InlineArray' is only available in}}
// expected-note@-2 {{add '@available' attribute to enclosing global function}}

@available(SwiftStdlib 9999, *)
func baz(x: InlineArray<3, Int>) {}

@available(SwiftStdlib 9999, *)
func qux(x: [3 of Int]) {}
