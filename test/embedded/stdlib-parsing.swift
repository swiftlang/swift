// RUN: %target-run-simple-swift(-Osize -swift-version 5 -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    print(Int("42")!)
    // CHECK: 42
    print(Int("-123")!)
    // CHECK: -123
    print(Int("1000", radix: 16)!)
    // CHECK: 4096
  }
}
