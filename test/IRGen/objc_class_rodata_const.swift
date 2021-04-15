// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module-path %t/ResilientBaseClasses.swiftmodule %S/Inputs/ResilientBaseClasses.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -import-objc-header %S/Inputs/ObjCBaseClasses.h -emit-ir %s | %FileCheck %s
// REQUIRES: objc_interop


import ResilientBaseClasses

// CHECK: @_METACLASS_DATA_{{.*}}9PureSwift = {{.*}} constant {{.*}} section "__DATA, __objc_const"
// CHECK: @_DATA_{{.*}}9PureSwift = {{.*}} constant {{.*}} section "__DATA, __objc_const"
class PureSwift {
    var x: Int = 0
}

// CHECK: @_METACLASS_DATA_{{.*}}12PureSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_const"
// CHECK: @_DATA_{{.*}}12PureSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_const"
class PureSwiftSub: PureSwift {
    var y: Int = 0
}

// CHECK: @_METACLASS_DATA_{{.*}}12ObjCSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_const"
// CHECK: @_DATA_{{.*}}12ObjCSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_data"
class ObjCSwiftSub: ObjCBase {
    var z: Int = 0
}

// CHECK: @_METACLASS_DATA_{{.*}}17ResilientSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_const"
// CHECK: @_DATA_{{.*}}17ResilientSwiftSub = {{.*}} constant {{.*}} section "__DATA, __objc_data"
class ResilientSwiftSub: ResilientBase {
    var z: Int = 0
}
