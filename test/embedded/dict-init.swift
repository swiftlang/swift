// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

public func foo() {
    let d = Dictionary<Int, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
    print(d[10]!)
    print(d[20]!)
}

foo()

// CHECK: hello
// CHECK: world
