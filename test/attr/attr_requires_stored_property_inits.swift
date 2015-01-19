// RUN: %target-swift-frontend -parse %clang-importer-sdk %s -verify

import ObjectiveC

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

  @NSManaged var managed_property: Int

  init() {
    a = 0
    b = 0
    c = 0
    d = 0
    e = 0
    f = 0
    g = 0
    h = 0
    i = 0
    j = 0
  }
}

class RequiresInheritedBad : RequiresOkay { // expected-error{{class 'RequiresInheritedBad' has no initializers}}
  var b: Int // expected-error{{stored property 'b' requires an initial value}}
  @NSManaged var managed_property_sub: Int
}

// Diagnose attempts to use this attribute on a non-class.
@requires_stored_property_inits struct S { } // expected-error{{'requires_stored_property_inits' may only be used on 'class' declarations}}

@requires_stored_property_inits enum E { } // expected-error{{'requires_stored_property_inits' may only be used on 'class' declarations}}

@requires_stored_property_inits func f() { } // expected-error{{'requires_stored_property_inits' may only be used on 'class' declarations}}


@requires_stored_property_inits
class NSSomething : NSObject { 
  // expected-note@-1 {{class 'NSSomething' requires all stored properties to have initial values or use @NSManaged}}
  // expected-error@-2 {{class 'NSSomething' has no initializers}}
  var x: Int // expected-error{{stored property 'x' requires an initial value or should be @NSManaged}}
}
