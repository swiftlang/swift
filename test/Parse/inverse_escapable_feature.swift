// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics



struct S: ~Escapable {} // expected-error {{type 'Escapable' requires -enable-experimental-feature NonescapableTypes}}

func hello(_ t: some Escapable, _ u: any Escapable) {} // expected-error 2{{type 'Escapable' requires -enable-experimental-feature NonescapableTypes}}

protocol Whatever: Escapable {} // expected-error {{type 'Escapable' requires -enable-experimental-feature NonescapableTypes}}
