// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop -parse-as-library %s

// There was a bug where @objc without Foundation was only diagnosed if it
// appeared before any top-level expressions. The _1 and _2 variants of
// this test cover both cases.

@objc class Foo {} // expected-error {{@objc attribute used without importing module 'Foundation'}} expected-error {{only classes that inherit from NSObject can be declared '@objc'}} {{1-7=}}
// expected-note@-1 {{inherit from 'NSObject' to silence this error}} {{16-16=: NSObject}}

#if false
#endif
