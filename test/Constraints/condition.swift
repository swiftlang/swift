// RUN: %target-parse-verify-swift

// Basic support for Bool
func simpleIf(_ b: Bool) {
  if b { }
}

// Support for non-Bool logic values
struct OtherLogicValue : Boolean {
  var boolValue: Bool { return true }
}

func otherIf(_ b : OtherLogicValue) {
  if b { }
}

// Support for arbitrary logic values in generics
func doIf<T : Boolean>(_ t: T) {
  if t { }
}
doIf(true)

// Using Boolean-ness to resolve overloading.
func getValue() -> OtherLogicValue {}
func getValue() -> Int {}

func testLogicValueOverloading() {
  if getValue() { }
}
