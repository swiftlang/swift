// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

enum MyEnum: String {
    case foo
    case bar
}

var e = MyEnum.foo
print(e.rawValue)
e = MyEnum.bar
print(e.rawValue)
// CHECK: foo
// CHECK: bar
