// RUN: %target-typecheck-verify-swift -swift-version 3

// Dollar is allowed as an identifier head in Swift 3.

func dollarVar() {
  var $ : Int = 42 // No error
  $ += 1
  print($)
}
func dollarLet() {
  let $ = 42 // No error
  print($)
}
func dollarClass() {
  class $ {} // No error
}
func dollarEnum() {
  enum $ {} // No error
}
func dollarStruct() {
  struct $ {} // No error
}

