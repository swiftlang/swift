// RUN: %target-typecheck-verify-swift -swift-version 5

extension DefaultStringInterpolation {
    @available(*, deprecated) func appendInterpolation(deprecated: Int) {}
}

// Make sure diagnostics emitted via string interpolations have a reasonable source location

_ = "\(deprecated: 42)" // expected-warning@:7 {{'appendInterpolation(deprecated:)' is deprecated}}

_ = "hello, world\(deprecated: 42)!!!" // expected-warning@:19 {{'appendInterpolation(deprecated:)' is deprecated}}

_ = "\(42)\(deprecated: 42)test\(deprecated: 42)"
// expected-warning@-1:12 {{'appendInterpolation(deprecated:)' is deprecated}}
// expected-warning@-2:33 {{'appendInterpolation(deprecated:)' is deprecated}}
_ = """
This is a multiline literal with a deprecated interpolation:

\(deprecated: 42)
"""
// expected-warning@-2:2 {{'appendInterpolation(deprecated:)' is deprecated}}
