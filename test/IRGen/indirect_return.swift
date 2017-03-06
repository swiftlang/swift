// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

// CHECK: define hidden swiftcc void @_T015indirect_return11generic_get{{[_0-9a-zA-Z]*}}F
func generic_get<T>(p: UnsafeMutablePointer<T>) -> T {
  // CHECK-NOT: [[T0:%.*]] = call i8* @_TFVs20UnsafeMutablePointerl6memoryQ_(i8* %1, %swift.type* %T)
  // CHECK: [[T1:%.*]] = bitcast i8* {{%.*}} to %swift.opaque*
  // CHECK: call %swift.opaque* {{%.*}}(%swift.opaque* %0, %swift.opaque* [[T1]], %swift.type* %T)
  return p.pointee
}
