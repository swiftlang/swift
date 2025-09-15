// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -O -emit-module -o %t/Module.swiftmodule %t/Module.swift -enable-experimental-feature Embedded -import-objc-header %t/header.h

// RUN: %target-swift-frontend -O -emit-ir %t/Main.swift -I%t -enable-experimental-feature Embedded -import-objc-header %t/header.h

// REQUIRES: swift_in_compiler
// REQUIRES: CPU=arm64e
// REQUIRES: swift_feature_Embedded

// BEGIN header.h

#pragma once
struct MyStruct {
    void (*fptr1)(void);
    void (*__ptrauth(0, 0, 0x4242) fptr2)(void);
};

// BEGIN Module.swift

public func test1(x: UnsafePointer<MyStruct>) {
    x.pointee.fptr1()
}

public func test2(x: UnsafePointer<MyStruct>) {
    x.pointee.fptr2()
}

// BEGIN Main.swift

import Module

public func mainfunc(x: UnsafePointer<MyStruct>) {
    test1(x: x)
    test2(x: x)
}

// CHECK: define {{.*}}@"$e4Main5test11xySPySo8MyStructVG_tF"
// CHECK:   call {{.*}}[ "ptrauth"(i32 0, i64 0) ]
// CHECK: define {{.*}}@"$e4Main5test21xySPySo8MyStructVG_tF"
// CHECK:   call {{.*}}[ "ptrauth"(i32 0, i64 16962) ]
