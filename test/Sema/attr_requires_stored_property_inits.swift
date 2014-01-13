// RUN: %swift -parse %s -verify

// Attribute requires that all stored properties have in-class
// initializers.
@requires_stored_property_inits
class RequiresOkay { // expected-note{{superclass 'RequiresOkay' requires all stored properties to have initial values}}
  var a = 5  
}

// Diagnose missing initializers.
@requires_stored_property_inits
class RequiresBad { // expected-note 4{{class 'RequiresBad' requires all stored properties to have initial values}}
  var a: Int // expected-error{{stored property 'a' requires an initial value}}
  var (b, c): (Int, Int) // expected-error{{stored properties 'b' and 'c' require initial values}}
  var (d, e, f): (Int, Int, Int) // expected-error{{stored properties 'd', 'e', and 'f' require initial values}}
  var (g, h, i, j): (Int, Int, Int, Int) // expected-error{{stored properties 'g', 'h', 'i', and others require initial values}}
}

class RequiresInheritedBad : RequiresOkay {
  var a: Int // expected-error{{stored property 'a' requires an initial value}}
}

// Diagnose attempts to use this attribute on a non-class.
@requires_stored_property_inits struct S { } // expected-error{{'requires_stored_property_inits' attribute can only be applied to a class}}

@requires_stored_property_inits enum E { } // expected-error{{'requires_stored_property_inits' attribute can only be applied to a class}}

@requires_stored_property_inits func f() { } // expected-error{{'requires_stored_property_inits' attribute can only be applied to a class}}


