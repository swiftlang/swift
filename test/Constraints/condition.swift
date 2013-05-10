// RUN: %swift -parse %s -verify

// Basic support for Bool
func simpleIf(b : Bool) {
  if b { }
}

// Support for non-Bool logic values
struct OtherLogicValue : LogicValue {
  func getLogicValue() -> Bool { return true }
}

func otherIf(b : OtherLogicValue) {
  if b { }
}

// Support for arbitrary logic values in generics
func doIf<T : LogicValue>(t : T) {
  if t { }
}
doIf(true)

// Using LogicValue-ness to resolve overloading.
func getValue() -> OtherLogicValue {}
func getValue() -> Int {}

func testLogicValueOverloading() {
  if getValue() { }  
}
