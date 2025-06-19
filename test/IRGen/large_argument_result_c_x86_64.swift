// RUN: %target-swift-frontend %S/large_argument_result_c.swift -Osize -import-objc-header %S/Inputs/large_argument_result_c.h -emit-ir -o - 2>&1 | %FileCheck %s

// REQUIRES: PTRSIZE=64

// Windows has a different ABI (not byval)
// UNSUPPORTED: OS=windows-msvc

// Whether llvm can remove the first two memcmp's dependents on the ABI (arm64's
// PCS, says stack arguments might be written to; x86-64 ABI copies indirect
// parameters for the call)
// REQUIRES: CPU=x86_64

// CHECK: define {{.*}}swiftcc void @"$s23large_argument_result_c7runTestyySo0A6_thingaF"(ptr {{.*}} %0)
// CHECK:  [[CALL_ALLOCA:%.*]] = alloca <{ %Ts6UInt64V, %Ts6UInt64V, %Ts6UInt64V
// CHECK:  call void @pass_and_return(ptr {{.*}} [[CALL_ALLOCA]], ptr nonnull byval{{.*}} %0, ptr nonnull byval{{.*}} %0)
// CHECK:  call {{.*}} @swift_allocObject
// CHECK:  [[BOX:%.*]] = {{.*}}call noalias ptr @swift_allocObject(
// CHECK:  [[ADDR_IN_BOX:%.*]] = getelementptr inbounds{{.*}} i8, ptr [[BOX]], i64 16
// CHECK:  call void @llvm.memcpy.p0.p0.i64(ptr {{.*}} [[ADDR_IN_BOX]], ptr {{.*}} [[CALL_ALLOCA]], i64 128, i1 false)
// CHECK:  call void @llvm.lifetime.end.p0(i64 128, ptr nonnull [[CALL_ALLOCA]])
public func runTest(_ l : large_thing) {
  let r = pass_and_return(l, l)
  print(r)
}
