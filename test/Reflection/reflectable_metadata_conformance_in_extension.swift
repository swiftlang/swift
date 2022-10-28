// RUN: %target-swift-frontend -enable-upcoming-feature OptInReflection -typecheck %s -verify

// MARK: - Errors
struct Foo {}

extension Foo: Reflectable {}  // expected-error {{extensions cannot declare protocol conformance to Reflectable}}