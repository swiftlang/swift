// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest -target %target-cpu-apple-macosx11
// REQUIRES: OS=macosx

@available(macOS 12.0, *)
struct X { }

@expression macro m1: X = A.B // expected-error{{'X' is only available in macOS 12.0 or newer}}

@available(macOS 12.0, *)
@expression macro m2: X = A.B
