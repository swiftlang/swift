// RUN: %target-typecheck-verify-swift -enable-experimental-feature ValueGenerics 

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_ValueGenerics

struct A<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                        // expected-note@-1 {{add @available attribute to enclosing generic struct}}

class B<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                       // expected-note@-1 {{add @available attribute to enclosing generic class}}

enum C<let N: Int> {} // expected-error {{values in generic types are only available in macOS 99.99.0 or newer}}
                      // expected-note@-1 {{add @available attribute to enclosing generic enum}}

func something<let N: Int>(_: A<N>) {} // OK, because A can't reference value generics.
