// RUN: %target-swift-frontend -enable-stack-protector -emit-ir %s -o - | %FileCheck %s

@_silgen_name("escape")
func f<T>(_ arg: UnsafePointer<T>)

public func escapeGenericValue<T>(_ t: T) {
    withUnsafePointer(to: t) { ptr in
       f(ptr)
    }
}

// CHECK: define {{.*}}swiftcc void @"$s15stack_protector21requestStackProtectoryyF"() [[SSPATTRS:#[0-9]+]] {
public func requestStackProtector() {
    var x: Int = 0
    escapeGenericValue(x)
}

// CHECK: [[SSPATTRS]] = { sspstrong {{.*}}"stack-protector-buffer-size"="8"
