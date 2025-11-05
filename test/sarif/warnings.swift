// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/warnings.sarif %t/output.sarif %s

func testWarning() {
  var unusedVar = 42  // warning: variable 'unusedVar' was never used
}
