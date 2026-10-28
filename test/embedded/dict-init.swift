// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

public func foo() {
    let d = Dictionary<Int, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
    print(d[10]!)
    print(d[20]!)
}

foo()

// CHECK: hello
// CHECK: world
