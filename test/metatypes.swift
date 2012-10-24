// RUN: %swift %s -verify

struct Test0 {}

var test0 : Test0.metatype
test0 = Test0

class Test1a { func foo() }
class Test1b : Test1a { }

// FIXME: all of these diagnostics are wrong
Test1b.foo() // expected-error {{invalid conversion}} expected-note{{while converting}}
var test1 = Test1a.metatype  // expected-error {{expected field name}}
test1 = Test1b.metatype
