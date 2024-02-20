// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics



struct S: ~Escapable {} // expected-error {{type '~Escapable' requires -enable-experimental-feature NonescapableTypes}}
