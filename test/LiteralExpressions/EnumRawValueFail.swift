// Enum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature LiteralExpressions -verify -verify-ignore-unrelated

enum E1: Int {
    case a = 2 + Int.random(in: 0 ..< 10)
    // expected-error@-1 {{not supported in a literal expression}}
    // expected-error@-2 {{raw value for enum case must be an integer literal expression}}
    case b
    case c
}

enum E2: Int {
    case a = 2.0
    // expected-error@-1 {{cannot convert value of type 'Double' to raw type 'Int'}}
    // expected-error@-3 {{'E2' declares raw type 'Int', but does not conform to RawRepresentable and conformance could not be synthesized}}
    // expected-note@-4 {{add stubs for conformance}}
}

enum E3: Int {
    case a = 2 + 2.0
    // expected-error@-1 {{cannot convert value of type 'Double' to raw type 'Int'}}
    // expected-error@-2 {{raw value for enum case must be an integer literal expression}}
    case b
    case c
}
