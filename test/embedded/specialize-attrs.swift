// RUN: %target-run-simple-swift(-parse-as-library -Onone -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -Osize -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

@_semantics("optimize.sil.specialize.generic.never")
func foo<T>(_ t: T) -> Int {
  return 42
}

@_semantics("optimize.sil.specialize.generic.size.never")
func foo2<T>(_ t: T) -> Int {
  return 42
}

@main
struct Main {
  static func main() {
    foo(42)
    foo2(42)
    print("OK!")
  }
}

// CHECK: OK!
