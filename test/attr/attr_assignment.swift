// RUN: %swift %s -verify

// Assignment operators
operator infix +- {
  precedence 90
  associativity left
}
@assignment func +-() {} // expected-error {{operators must have one or two arguments}} expected-error {{assignment operator must have an initial inout argument}}
@assignment func +-(x: Int) {} // expected-error {{assignment operator must have an initial inout argument}} expected-error {{unary operator implementation must have a 'prefix' or 'postfix' attribute}}
@assignment func +-(inout x: Int) {} // expected-error {{unary operator implementation must have a 'prefix' or 'postfix' attribute}}
@assignment func +-(x: Int, y: Int) {} // expected-error {{assignment operator must have an initial inout argument}}
@assignment func +-(inout x: Int, y: Int) {} // no-error
@assignment func assign(x: Int, y: Int) {} // expected-error {{'assignment' attribute cannot be applied to this declaration}}

func use_assignments(var i: Int, var j: Int) {
 ++i
 i += j
 ++(&i)  // expected-error {{could not find an overload for '++' that accepts the supplied arguments}}
 &i += j // expected-error {{could not find an overload for '+=' that accepts the supplied arguments}}
}


