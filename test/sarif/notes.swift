// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/notes.sarif %t/output.sarif %s

class MyClass {
  func myMethod() {}
}

func testNotes() {
  let obj = MyClass()
  obj.myMethod  // error: cannot reference instance method without parentheses
                // note: add parentheses to call the method
}
