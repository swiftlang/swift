// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/errors.sarif %t/output.sarif %s

let a = 5
a = 10  // error: cannot assign to value: 'a' is a 'let' constant

let b: Int = "string"  // error: cannot convert value of type 'String' to specified type 'Int'

func foo() -> Int {
  // error: missing return in a function expected to return 'Int'
}

func testLocation() {
  let x = 5
  let y = x + "string"  // error at line 8, column 17
}
