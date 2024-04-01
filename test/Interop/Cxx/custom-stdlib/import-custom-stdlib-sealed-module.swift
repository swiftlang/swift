// RUN: rm -rf %t
// RUN: %target-swiftxx-frontend -DCUSTOM -emit-module -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs %s -module-name Custom -o %t/Custom.swiftmodule
// RUN: %target-swiftxx-frontend -emit-ir -I %t -I %S/Inputs %s -o - | %FileCheck %s

#if CUSTOM

@_implementationOnly import CxxHeader
@_implementationOnly import custom_std

public func testCustomString() -> Int {
    let s = std.string("hello")
    return Int(s.size())
}

#else

import Custom

public func test() -> Int {
    return testCustomString()
}

// CHECK: call swiftcc i64 @"$s6Custom04testA6StringSiyF"()

#endif
