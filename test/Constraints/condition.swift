// RUN: %target-parse-verify-swift

// Basic support for Bool
func simpleIf(b: Bool) {
  if b { }
}

// Support for non-Bool logic values
struct OtherLogicValue : BooleanType {
  var boolValue: Bool { return true }
}

func otherIf(b : OtherLogicValue) {
  if b { }
}

// Support for arbitrary logic values in generics
func doIf<T : BooleanType>(t: T) {
  if t { }
}
doIf(true)

// Using BooleanType-ness to resolve overloading.
func getValue() -> OtherLogicValue {}
func getValue() -> Int {}

func testLogicValueOverloading() {
  if getValue() { }
}
