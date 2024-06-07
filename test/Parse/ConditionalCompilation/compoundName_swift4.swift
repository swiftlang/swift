// RUN: %target-typecheck-verify-swift -swift-version 4

/// Reject compound names.
#if BAR(_:) // expected-error@:5 {{invalid conditional compilation expression}}
#elseif os(x:)(macOS) // expected-error@:9 {{unexpected platform condition (expected 'os', 'arch', or 'swift')}}
#elseif os(Linux(foo:bar:)) // expected-error@:12 {{unexpected platform condition argument: expected identifier}}
#endif
