// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/fixits.sarif %t/output.sarif %s

func testFixIt() {
  var x: Int
  print(x)  // error: variable 'x' used before being initialized
            // fix-it: initialize 'x' with a value
}

struct Point {
  var x: Int
  var y: Int
}

let p = Point()  // error: missing arguments for parameters 'x', 'y' in call
                 // fix-it: insert ', x: <#Int#>, y: <#Int#>'
