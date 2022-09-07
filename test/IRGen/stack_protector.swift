// RUN: %target-swift-frontend -enable-stack-protector -Onone -emit-ir %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -enable-stack-protector -O -emit-ir %s -o - | %FileCheck %s

// REQUIRES: swift_in_compiler

@_silgen_name("escape")
func f(_ arg: UnsafePointer<Int>)

@_silgen_name("noescape")
func g(_ arg: Int)

// CHECK: define {{.*}}swiftcc void @"$s15stack_protector21requestStackProtectoryyF"() [[SSPATTRS:#[0-9]+]] {
public func requestStackProtector() {
    var x: Int = 0
    f(&x)
}

// CHECK-NOT: define {{.*}}swiftcc void @"$s15stack_protector16noStackProtectoryyF"() [[SSPATTRS]] {
public func noStackProtector() {
    var x: Int = 27
    g(x)
    g(x) // avoid function merging by calling `g` two times
}

// CHECK: [[SSPATTRS]] = { sspstrong {{.*}}"stack-protector-buffer-size"="8"
