// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test

private struct S {
  let string: String

  lazy var lazyString: String = { string }()
}

func test() {
  var s = S(string: "a")
  precondition(s.lazyString == "a")
  s = S(string: "b")
  precondition(s.lazyString == "b")
}

test()
