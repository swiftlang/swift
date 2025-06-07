// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -lto=llvm-full %lto_flags -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// Check that initializing a Double with an integer literal doesn't result in unresolved symbols
@inline(never)
func testLiteral() -> Double {
  return Double(1)
} 

@main
struct Main {
  static func main() {
    print(testLiteral() == 1.0)
    // CHECK: true
  }
}
