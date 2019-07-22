// Parse-only modes, like swift -parse and SourceKit, should not emit strange
// interpolation errors in `#if swift(>=5)` blocks. (SR-9937)
//
// Even though this is in the test/SourceKit directory, it also tests
// -frontend -parse behavior because the test cases are exactly the same.

// RUN: %sourcekitd-test -req=open %s -- -swift-version 4.2 %s == -req=print-diags %s | %FileCheck %s
// RUN: %target-swift-frontend -parse -verify -swift-version 4.2 %s

let x = 1

// We should not warn in blocks that can only run when `swift(>=5.0)`.

#if swift(>=5.0)
  print("[\(foo: x)]")
  print("[\(x, x)]")
#endif

// CHECK: <<NULL>>
