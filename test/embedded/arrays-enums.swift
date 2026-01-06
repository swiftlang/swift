// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

enum Node {
  indirect case inner(Node, Node)
  case leaf(Int)
}

@main
struct Main {
  static func main() {
    _ = [Node.leaf(42), Node.leaf(42)]
    print("OK!")
    // CHECK: OK!
  }
}
