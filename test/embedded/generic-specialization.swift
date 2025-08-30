// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

func f<V>(_ a: [V]) -> [String] {
    return a.indices.map { String($0 /* as Int*/) }  // adding `as Int` makes it compile
}

// CHECK: 0
print(f(["A"]).joined())

