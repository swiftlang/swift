// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    var str = "hello "
    for _ in 0 ..< 4 { str = str + str }
    print(str)
  }
}

// CHECK: hello hello hello hello hello hello hello hello hello hello hello hello hello hello hello hello 
