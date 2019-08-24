// RUN: %swift -typecheck %s -verify
// rdar://16172507
// XFAIL: *

// This checks if adding ".metatype" is valid or not.
// Any change here, should be reflected in code-completion results.

class Foo {}

var fooTy1 = Foo.metatype // expected-error {{type 'Foo' has no member 'metatype'}}

var fooTy2 : Foo.metatype

var foo : Foo
var fooTy3 = foo.metatype // expected-error {{value of type 'Foo' has no member 'metatype'}}
