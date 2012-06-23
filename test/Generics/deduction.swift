// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Deduction of generic arguments
//===----------------------------------------------------------------------===//

func identity<T>(value : T) -> T { return value }

func useIdentity(x : Int, y : Float, i32 : Int32) {
  x = identity(x)
  y = identity(y)

  // FIXME: These need much smarter deduction.
//  x = identity(17)
//  i32 = identity(17)
}

// FIXME: Crummy diagnostic!
func twoIdentical<T>(x : T, y : T) -> T {} // expected-note{{found this candidate}}

func useTwoIdentical(x : Int, y : Float, i32 : Int32) {
  x = twoIdentical(x, x)
  y = twoIdentical(y, y)
  x = twoIdentical(x, 1)
  x = twoIdentical(1, x)
  y = twoIdentical(1.0, y)
  y = twoIdentical(y, 1.0)
  
  twoIdentical(x, y) // expected-error{{no candidates found for call}}
}
