// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: noncopyable_generics

struct S: ~Escapable {} // expected-error {{type '~Escapable' requires -enable-experimental-feature NonescapableTypes}}
