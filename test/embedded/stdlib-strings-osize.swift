// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let str = "Hello Hello This Is A Long String"
    print(str)
  }
}

// CHECK: Hello Hello This Is A Long String
