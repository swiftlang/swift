// REQUIRES: swift_swift_parser
// RUN: %swift-parse-test -swift-parser -lib-parse -skip-bodies -n 10 %s

struct S {
  func foo() {
    print(1)
  }
}
