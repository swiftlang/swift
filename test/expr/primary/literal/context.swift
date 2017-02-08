// RUN: %target-typecheck-verify-swift

// Test "context" literals, #file, #line, #column, etc.

func requireInt(_ x: Int) { }
func requireString(_ s: String) { }

func testContextLiterals() {
  let file = #file
  requireString(file)
  let line = #line
  requireInt(line)
  let column = #column
  requireInt(column)
  let function = #function
  requireString(function)
}

// Disambiguate between collection literals and object literals.
func collectionLiteralDisambiguation() {
  _ = [#line]
  _ = [#line, #column]
  _ = [#line : #column]
}
