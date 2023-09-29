// RUN: %target-swift-frontend -O -emit-irgen %s %S/Inputs/print.swift -module-name main -parse-as-library -enable-experimental-feature Embedded | %FileCheck %s --check-prefix CHECK-IR
// RUN: %target-run-simple-swift(-O %S/Inputs/print.swift -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

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
