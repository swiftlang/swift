// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo -O -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: swift_feature_Embedded

enum MyEnum: String {
    case case1
    case case2
    case case3
    case case4
    case case5
    case case6
    case case7
    case case8
    case case9
    case case10
    case case11
    case case12
    case case13
    case case14
    case case15
    case case16
    case case17
    case case18
    case case19
}

var e = MyEnum.case1
print(e.rawValue)
e = MyEnum.case2
print(e.rawValue)
// CHECK: case1
// CHECK: case2
