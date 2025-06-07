// RUN: %target-run-simple-swift(-Osize -swift-version 5 -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    print("string: \("string")")
    // CHECK: string: string

    print("static string: \("string" as StaticString)")
    // CHECK: static string: string

    print("integers: \(42) \(-42) \(0xffffffff)")
    // CHECK: integers: 42 -42 4294967295
    
    print("booleans: \(true) \(false)")
    // CHECK: booleans: true false
  }
}
