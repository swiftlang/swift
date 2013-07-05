// RUN: %swift -parse %s -verify

// Verify the use of unbound generic types. They are permitted in
// certain places where type inference can fill in the generic
// arguments, and banned everywhere else.

// --------------------------------------------------
// Places where generic arguments are always required
// --------------------------------------------------

struct Foo<T> { // expected-note{{generic type 'Foo' declared here}} expected-note{{generic type 'Foo' declared here}}
  struct Wibble { }
}

class Dict<K, V> { } // expected-note{{generic type 'Dict' declared here}} expected-note{{generic type 'Dict' declared here}} expected-note{{generic type 'Dict' declared here}}

// Cannot alias a generic type without arguments.
typealias Bar = Foo // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}

// Cannot refer to a member of a generic type without arguments.
typealias FW = Foo.Wibble // expected-error{{reference to generic type 'Foo' requires arguments in <...>}}

// Cannot inherit from a generic type without arguments.
class MyDict : Dict { } // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}

// Cannot create variables of a generic type without arguments.
// FIXME: <rdar://problem/14238814> would allow it for local variables
// only
var x : Dict // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}

// Cannot create parameters of generic type without arguments.
func f(x : Dict) {} // expected-error{{reference to generic type 'Dict' requires arguments in <...>}}



