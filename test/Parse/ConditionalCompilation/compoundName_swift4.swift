// RUN: %target-typecheck-verify-swift -swift-version 4

/// Reject compound names.
#if BAR(_:) // expected-error@:5 {{invalid conditional compilation expression}}
#elseif os(x:)(macOS) // expected-error@:9 {{invalid conditional compilation expression}}
#elseif os(Linux(foo:bar:)) // expected-error@:9 {{'os' requires a single unlabeled argument for the operating system}}
#endif
