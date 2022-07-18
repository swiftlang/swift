// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s

// CHECK: define{{.*}}swiftcc void @swapPointers({{.*}}noalias{{.*}},{{.*}}noalias{{.*}})
@_silgen_name("swapPointers")
public func swapPointers<T>(_ lhs: inout UnsafePointer<T>, _ rhs: inout UnsafePointer<T>) {}
