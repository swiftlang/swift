// RUN: %target-typecheck-verify-swift -enable-experimental-feature ValueGenerics -enable-experimental-feature NonescapableTypes

// REQUIRES: OS=macosx

struct A<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                        // expected-note@-1 {{add @available attribute to enclosing generic struct}}

class B<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                       // expected-note@-1 {{add @available attribute to enclosing generic class}}

enum C<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                      // expected-note@-1 {{add @available attribute to enclosing generic enum}}

func something<let N: Int>(_: A<N>) {} // OK, because A can't reference value generics.
