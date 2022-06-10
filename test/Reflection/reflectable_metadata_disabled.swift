// RUN: %target-swift-frontend -disable-reflection-metadata -typecheck %s -verify

// MARK: - Errors
protocol P: Reflectable {} // expected-error {{reflection metadata emission must be enabled for modules with `Reflectable` declarations}}

enum E: Reflectable {} // expected-error {{reflection metadata emission must be enabled for modules with `Reflectable` declarations}}

class C: Reflectable {} // expected-error {{reflection metadata emission must be enabled for modules with `Reflectable` declarations}}
