// REQUIRES: swift_swift_parser
// RUN: %swift-parse-test -swift-parser -lib-parse -skip-bodies -n 2 %s
// REQUIRES: rdar117750086
struct S {
  func foo() {
    print(1)
  }
}
