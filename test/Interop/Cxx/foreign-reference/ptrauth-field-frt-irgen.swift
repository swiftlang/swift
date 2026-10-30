// RUN: %swift %s -emit-ir -O -disable-llvm-merge-functions-pass -target arm64e-apple-macos14.0 \
// RUN:   -cxx-interoperability-mode=default -I %S/Inputs/ | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Regression test for rdar://190639894: __ptrauth-qualified fields of C++
// foreign reference types must be authenticated on load and signed on store,
// matching what Clang does for the same field.
//
// Data pointers are unsigned in Swift, so a load only authenticates
// (ptrauth.auth) and a store only signs (ptrauth.sign). C function pointers are
// re-signed between the field's schema and Swift's function pointer schema.

import PtrAuthFieldFRT

// RefBox.p: DA key (2), address-diversified, discriminator
// ptrauth_string_discriminator("RefBox.p").

// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s{{.*}}readData{{.*}}F"(ptr %0)
// CHECK:   [[FIELD:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 8
// CHECK:   [[SIGNED:%.*]] = load ptr, ptr [[FIELD]]
// CHECK:   [[ADDR:%.*]] = ptrtoint ptr [[FIELD]] to i64
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[ADDR]], i64 [[RefBoxP:[0-9]+]])
// CHECK:   [[SIGNEDINT:%.*]] = ptrtoint ptr [[SIGNED]] to i64
// CHECK:   [[AUTHED:%.*]] = {{.*}}call i64 @llvm.ptrauth.auth(i64 [[SIGNEDINT]], i32 2, i64 [[DISC]])
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK-NOT: @llvm.ptrauth.sign
// CHECK:   [[PTR:%.*]] = inttoptr i64 [[AUTHED]] to ptr
// CHECK:   load i32, ptr [[PTR]]
// CHECK: }
public func readData(_ b: RefBox) -> Int32 {
  return unsafe b.p!.pointee
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{.*}}writeData{{.*}}F"(ptr %0, ptr %1)
// CHECK:   [[FIELD:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 8
// CHECK:   [[ADDR:%.*]] = ptrtoint ptr [[FIELD]] to i64
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[ADDR]], i64 [[RefBoxP]])
// CHECK:   [[RAW:%.*]] = ptrtoint ptr %1 to i64
// CHECK:   {{.*}}call i64 @llvm.ptrauth.sign(i64 [[RAW]], i32 2, i64 [[DISC]])
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK-NOT: @llvm.ptrauth.auth
// CHECK:   store ptr {{.*}}, ptr [[FIELD]]
// CHECK: }
public func writeData(_ b: RefBox, _ q: UnsafeMutablePointer<Int32>) {
  unsafe b.p = q
}

// A non-nullable field is imported as a non-optional pointer.

// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s{{.*}}readNonnullData{{.*}}F"(ptr %0)
// CHECK:   [[FIELD:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 24
// CHECK:   {{.*}}call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK: }
public func readNonnullData(_ b: RefBox) -> Int32 {
  return unsafe b.nn.pointee
}

// RefBox.fn: IA key (0), address-diversified, discriminator 77. It is re-signed
// to Swift's C function pointer schema (IA key, discriminator 0).

// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{.*}}writeFunction{{.*}}F"(ptr %0, ptr %1)
// CHECK:   [[FIELD:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 16
// CHECK:   [[ADDR:%.*]] = ptrtoint ptr [[FIELD]] to i64
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[ADDR]], i64 77)
// CHECK:   [[RAW:%.*]] = ptrtoint ptr %1 to i64
// CHECK:   {{.*}}call i64 @llvm.ptrauth.resign(i64 [[RAW]], i32 0, i64 0, i32 0, i64 [[DISC]])
// CHECK: }
public func writeFunction(_ b: RefBox, _ f: @convention(c) () -> Int32) {
  b.fn = f
}

// Key path accessors for a ptrauth field authenticate and sign as well.

// CHECK-LABEL: define {{.*}}swiftcc {{.*}} @"$s{{.*}}keyPathRead{{.*}}F"(ptr %0)
// CHECK:   {{.*}}call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK: }
public func keyPathRead(_ b: RefBox) -> UnsafeMutablePointer<Int32>? {
  return unsafe b[keyPath: \RefBox.p]
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{.*}}keyPathWrite{{.*}}F"(ptr %0, ptr %1)
// CHECK:   {{.*}}call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK: }
public func keyPathWrite(_ b: RefBox, _ q: UnsafeMutablePointer<Int32>?) {
  unsafe b[keyPath: \RefBox.p] = q
}

// The same applies to C structs, both for by-value reads and key paths.

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s{{.*}}readCBoxByValue{{.*}}F"(ptr
// CHECK:   {{.*}}call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK: }
public func readCBoxByValue(_ b: CBox) -> UnsafeMutablePointer<Int32>? {
  return unsafe b.p
}

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s{{.*}}readCBoxKeyPath{{.*}}F"(ptr
// CHECK:   {{.*}}call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK: }
public func readCBoxKeyPath(_ b: CBox) -> UnsafeMutablePointer<Int32>? {
  return unsafe b[keyPath: \CBox.p]
}
