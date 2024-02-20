// RUN: %target-swift-frontend -O -emit-irgen %s -module-name main -parse-as-library -enable-experimental-feature Embedded | %FileCheck %s --check-prefix CHECK-IR
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

public func stringArray() -> [StaticString] {
  return ["a", "b", "c", "d"]
}
// CHECK-IR:      define {{.*}}@"$s4main11stringArraySays12StaticStringVGyF"
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT:   call {{.*}}@swift_initStaticObject

@main
struct Main {
  static func main() {
    for c in stringArray() {
      print(c)
      // CHECK: a
      // CHECK: b
      // CHECK: c
      // CHECK: d
    }
  }
}
