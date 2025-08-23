// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://143586718 - crash due to lookup not delayed on partially resolved base with Sendable requirements

_ = { () -> Array? in .max }
// expected-error@-1 {{unable to infer closure type without a type annotation}}
