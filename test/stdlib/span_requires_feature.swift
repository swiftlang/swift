// RUN: %target-typecheck-verify-swift -verify-additional-prefix missing-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Span
// REQUIRES: swift_feature_Span

@available(SwiftStdlib 6.1, *)
func f(_: Span<Int>) { }
// expected-missing-error@-1{{'Span' requires -enable-experimental-feature Span}}


@available(SwiftStdlib 6.1, *)
func g(_: RawSpan) { }
// expected-missing-error@-1{{'RawSpan' requires -enable-experimental-feature Span}}
