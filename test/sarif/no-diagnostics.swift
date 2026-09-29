// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/no-diagnostics.sarif %t/output.sarif %s

// Test valid code with no diagnostics - should produce empty results

func validFunction() -> Int {
  return 42
}

let validVariable = "Hello, World!"

struct ValidStruct {
  var property: String
}
