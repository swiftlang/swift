// RUN: %target-typecheck-verify-swift -swift-version 4

/// Reject compound names.
#if BAR(_:) // expected-error {{invalid conditional compilation expression}}
#elseif os(x:)(macOS) // expected-error {{unexpected platform condition (expected 'os', 'arch', or 'swift')}}
#elseif os(Linux(foo:bar:)) // expected-error {{unexpected platform condition argument: expected identifier}}
#endif
