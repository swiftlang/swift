// Note we use '--implicit-check-not' to ensure we exhaustively match errors.
// RUN: not %target-swift-frontend -parse -experimental-skip-all-function-bodies -diagnostic-style=llvm %s 2>&1 | %FileCheck --implicit-check-not='error:' %s

// We cannot skip function bodies with #sourceLocation.
func foo() {
  // CHECK: [[@LINE+1]]:4: error: expected expression
  [;
  #sourceLocation(file: "A", line: 1)
  [;
  // CHECK: A:1:4: error: expected expression
}

func bar() {
  // CHECK: A:7:4: error: expected expression
  [;
#sourceLocation()
  [;
  // CHECK: [[@LINE-1]]:4: error: expected expression
}

// This function body is skipped.
func baz() {
  [;
}

// This member list and function are not skipped.
struct S {
  func qux() {
    // CHECK: [[@LINE+1]]:6: error: expected expression
    [;
    #sourceLocation(file: "B", line: 1)
    [;
    // CHECK: B:1:6: error: expected expression
  }
  func ;
  // CHECK: B:4:8: error: expected identifier
}

// This member list is also not skipped.
struct R {
  // CHECK: B:11:8: error: expected identifier
  func ;

#sourceLocation()

  func ;
  // CHECK: [[@LINE-1]]:8: error: expected identifier
}

// This member list is skipped.
struct Q {
    [;
}
