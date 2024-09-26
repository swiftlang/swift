// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@inline(never)
public func sum(_ v: SIMD4<Float>) -> Float {
    return v.x + v.y + v.z + v.w
}

@main
struct Main {
  static func main() {
    let v = SIMD4<Float>(1.0, 2.0, 3.0, 4.0)
    guard v.sum() == sum(v) else {
        fatalError()
    }

    // CHECK: success
    print("success")
  }
}
