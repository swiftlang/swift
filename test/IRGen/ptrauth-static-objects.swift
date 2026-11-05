// RUN: %swift -target arm64e-apple-ios18.0 -O -primary-file %s -emit-ir -module-name A | %FileCheck %s
// RUN: %swift -target arm64e-apple-ios17.0 -O -primary-file %s -emit-ir -module-name A | %FileCheck %s --check-prefix=RUNTIME-INIT

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// The runtime signs the isa it stores into an object with the ObjC isa schema
// and the address of the object. A read-only statically initialized array has
// its isa signed the same way at compile time.

// CHECK: @"$s1A9makeArraySaySiGyFTv_r" = {{.*}}constant {{.*}}{ ptr [[ISA:@"\$ss20__StaticArrayStorageCN\.ptrauth[^"]*"]], i64 ptrtoint (ptr @_swiftImmortalRefCount to i64) }
// CHECK: [[ISA]] = private constant { ptr, i32, i64, i64 } { ptr @"$ss20__StaticArrayStorageCN", i32 2, i64 ptrtoint (ptr @"$s1A9makeArraySaySiGyFTv_r" to i64), i64 27361 }, section "llvm.ptrauth"

// Before iOS 18 the runtime initializes the header of the object.
// RUNTIME-INIT: @"$s1A9makeArraySaySiGyFTv_" = internal global {{.*}} { [1 x i64] zeroinitializer, {{.*}} <{ %swift.refcounted zeroinitializer,
// RUNTIME-INIT-NOT: __StaticArrayStorageCN

public func makeArray() -> [Int] {
  return [1, 2, 3]
}
