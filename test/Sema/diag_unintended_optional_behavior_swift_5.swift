// RUN: %target-typecheck-verify-swift -swift-version 5

// Additional warnings produced in Swift 5+ mode.

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func takesOptionalAny(_: Any?, _: Any?) {}

class C {
  var a: Int!
  var b: Any?!
  func returningIUO() -> Int! { return a }
  func returningAny() -> Any { return a }

  subscript(i: Int) -> Int! { return 0 }
  subscript(i: Float) -> Any! { return 0 }
}

class D {
  init!() {}
}

func returningIUO() -> Int! { return 1 }

func warnIUOToAnyCoercion(_ a: Int!, _ b: Any?!) {
  _ = takeAny(a, b)
  _ = takeAny(returningIUO(), C().returningIUO())
  _ = takeAny(C().a, C().b)
  _ = takeAny(C()[0], C()[1.0])
  _ = takeAny(D(), D())

  _ = takeAny(a as Any, b as Any)
}

func warnIUOToOptionalAnyCoercion(_ a: Int!, _ b: Any?!, _ c: Int??!, _ d: Any???!) {
  takesOptionalAny(a, b)

  takesOptionalAny(a, b ?? "")
  takesOptionalAny(a, b!)
  takesOptionalAny(a, b as Any?)

  takesOptionalAny(c, d)

  takesOptionalAny(c!!, d!!!)
  takesOptionalAny(c as Any?, d as Any?)
}

func takesCollectionOfAny(_ a: [Any], _ d: [String : Any]) {}

func warnCollectionOfIUOToAnyCoercion(_ a: Int!) {
  takesCollectionOfAny([a], ["test" : a])
}

class FakeFooButton {}
weak var fakeButton : FakeFooButton!
var buttons : [Any] = []
buttons.append(fakeButton)
