// RUN: %swift %s -verify

@IBOutlet // expected-error {{only properties can be declared 'IBOutlet'}}
var iboutlet_global: Int

@IBOutlet // expected-error {{only properties can be declared 'IBOutlet'}}
class IBOutletClassTy {}
@IBOutlet // expected-error {{only properties can be declared 'IBOutlet'}}
struct IBStructTy {}

@IBOutlet // expected-error {{only properties can be declared 'IBOutlet'}}
func IBFunction() -> () {}

@objc
class IBOutletWrapperTy {
  @IBOutlet
  var value : IBOutletWrapperTy = IBOutletWrapperTy() // no-warning

  @IBOutlet
  class var staticValue: IBOutletWrapperTy = 52  // expected-error {{expression does not type-check}}
  // expected-error@-2 {{only properties can be declared 'IBOutlet'}}
  // expected-error@-2 {{static variables not yet supported in classes}}

  @IBOutlet // expected-error {{only properties can be declared 'IBOutlet'}}
  func click() -> () {}
}
