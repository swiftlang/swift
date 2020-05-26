// RUN: %target-typecheck-verify-swift -swift-version 4

// SR-1661: Dollar was accidentally allowed as an identifier in Swift 3.
// SE-0144: Reject this behavior in the future.

func dollarVar() {
  var $ : Int = 42 // expected-error {{'$' is not an identifier; use backticks to escape it}} {{7-8=`$`}}
  $ += 1 // expected-error {{'$' is not an identifier; use backticks to escape it}} {{3-4=`$`}}
  print($) // expected-error {{'$' is not an identifier; use backticks to escape it}} {{9-10=`$`}}
}
func dollarLet() {
  let $ = 42 // expected-error {{'$' is not an identifier; use backticks to escape it}} {{7-8=`$`}}
  print($) // expected-error {{'$' is not an identifier; use backticks to escape it}} {{9-10=`$`}}
}
func dollarClass() {
  class $ {} // expected-error {{'$' is not an identifier; use backticks to escape it}} {{9-10=`$`}}
}
func dollarEnum() {
  enum $ {} // expected-error {{'$' is not an identifier; use backticks to escape it}} {{8-9=`$`}}
}
func dollarStruct() {
  struct $ {} // expected-error {{'$' is not an identifier; use backticks to escape it}} {{10-11=`$`}}
}

func dollarFunc() {
  func $($ dollarParam: Int) {}
  // expected-error@-1 {{'$' is not an identifier; use backticks to escape it}} {{8-9=`$`}}
  // expected-error@-2 {{'$' is not an identifier; use backticks to escape it}} {{10-11=`$`}}
  $($: 24)
  // expected-error@-1 {{'$' is not an identifier; use backticks to escape it}} {{3-4=`$`}}
  // expected-error@-2 {{'$' is not an identifier; use backticks to escape it}} {{5-6=`$`}}
}

func escapedDollarVar() {
  var `$` : Int = 42 // no error
  `$` += 1
  print(`$`)
}
func escapedDollarLet() {
  let `$` = 42 // no error
  print(`$`)
}
func escapedDollarClass() {
  class `$` {} // no error
}
func escapedDollarEnum() {
  enum `$` {} // no error
}
func escapedDollarStruct() {
  struct `$` {} // no error
}

func escapedDollarFunc() {
  func `$`(`$`: Int) {} // no error
  `$`(`$`: 25) // no error
}

func escapedDollarAnd() {
  // FIXME: Bad diagnostics.
  `$0` = 1 // expected-error {{expected expression}}
  `$$` = 2
  `$abc` = 3
}

func $declareWithDollar() { // expected-error{{cannot declare entity named '$declareWithDollar'}}
  var $foo = 17 // expected-error{{cannot declare entity named '$foo'}}
  // expected-warning@-1 {{initialization of variable '$foo' was never used; consider replacing with assignment to '_' or removing it}}
  func $bar() { } // expected-error{{cannot declare entity named '$bar'}}
  func wibble(
    $a: Int, // expected-error{{cannot declare entity named '$a'}}
    $b c: Int) { } // expected-error{{cannot declare entity named '$b'}}
}
