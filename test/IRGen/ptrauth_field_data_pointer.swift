// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift -emit-ir -O -disable-llvm-merge-functions-pass -target arm64e-apple-macos14.0 \
// RUN:   -import-objc-header %t/header.h %t/test.swift | %FileCheck %t/test.swift --check-prefixes=CHECK,NOTD

// IRGen used to crash here with type-discriminated function pointers.
// RUN: %swift -emit-ir -O -disable-llvm-merge-functions-pass -target arm64e-apple-macos14.0 \
// RUN:   -Xcc -fptrauth-function-pointer-type-discrimination \
// RUN:   -import-objc-header %t/header.h %t/test.swift | %FileCheck %t/test.swift --check-prefixes=CHECK,TD

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

//--- header.h

#include <ptrauth.h>
#include <stdint.h>

#define SIGNED(d) \
  __ptrauth(ptrauth_key_process_independent_data, 1, ptrauth_string_discriminator(d))

struct opaque;

struct node {
  struct node *_Nullable SIGNED("node.parent") parent;
  void *_Nullable SIGNED("node.raw") raw;
  struct opaque *_Nullable SIGNED("node.op") op;
  struct node *_Nonnull SIGNED("node.nn") nn;
  uintptr_t SIGNED("node.ival") ival;
  void (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 77) fn)(void);
  int (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 31) vfn)(int, ...);
};

//--- test.swift

// Data pointers: loads only authenticate, stores only sign.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}readParent{{.*}}"(ptr %0)
// CHECK:   call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK-NOT: @llvm.ptrauth.sign
// CHECK: {{^}$}}
public func readParent(_ n: UnsafeMutablePointer<node>) -> UnsafeMutablePointer<node>? {
  return n.pointee.parent
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeParent{{.*}}"(ptr %0, ptr %1)
// CHECK:   call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK-NOT: @llvm.ptrauth.auth
// CHECK: {{^}$}}
public func writeParent(_ n: UnsafeMutablePointer<node>, _ p: UnsafeMutablePointer<node>) {
  n.pointee.parent = p
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}readRaw{{.*}}"(ptr %0)
// CHECK:   call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func readRaw(_ n: UnsafeMutablePointer<node>) -> UnsafeMutableRawPointer? {
  return n.pointee.raw
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeOpaque{{.*}}"(ptr %0, ptr %1)
// CHECK:   call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func writeOpaque(_ n: UnsafeMutablePointer<node>, _ p: OpaquePointer?) {
  n.pointee.op = p
}

// A non-nullable field is imported as a non-optional type.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}readNonnull{{.*}}"(ptr %0)
// CHECK:   call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func readNonnull(_ n: UnsafeMutablePointer<node>) -> UnsafeMutablePointer<node> {
  return n.pointee.nn
}

// A pointer-sized integer.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeInt{{.*}}"(ptr %0, i64 %1)
// CHECK:   call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func writeInt(_ n: UnsafeMutablePointer<node>, _ v: UInt) {
  n.pointee.ival = v
}

// C function pointers are re-signed between the field's schema and Swift's.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}readFunction{{.*}}"(ptr %0)
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 77)
// CHECK:   call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 [[DISC]], i32 0, i64 0)
// CHECK: {{^}$}}
public func readFunction(_ n: UnsafeMutablePointer<node>) -> (@convention(c) () -> Void)? {
  return n.pointee.fn
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeFunction{{.*}}"(ptr %0, ptr %1)
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 77)
// CHECK:   call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 0, i32 0, i64 [[DISC]])
// CHECK: {{^}$}}
public func writeFunction(_ n: UnsafeMutablePointer<node>, _ f: @escaping @convention(c) () -> Void) {
  n.pointee.fn = f
}

// Variadic function pointers are imported as OpaquePointer but still re-signed,
// with Clang's discriminator for int (*)(int, ...).

// CHECK-LABEL: define {{.*}} @"$s{{.*}}readVariadic{{.*}}"(ptr %0)
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 31)
// NOTD:    call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 [[DISC]], i32 0, i64 0)
// TD:      call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 [[DISC]], i32 0, i64 7476)
// CHECK: {{^}$}}
public func readVariadic(_ n: UnsafeMutablePointer<node>) -> OpaquePointer? {
  return n.pointee.vfn
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeVariadic{{.*}}"(ptr %0, ptr %1)
// CHECK:   [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 31)
// NOTD:    call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 0, i32 0, i64 [[DISC]])
// TD:      call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 0, i64 7476, i32 0, i64 [[DISC]])
// CHECK: {{^}$}}
public func writeVariadic(_ n: UnsafeMutablePointer<node>, _ f: OpaquePointer?) {
  n.pointee.vfn = f
}

// Key paths authenticate and sign too.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}readKeyPath{{.*}}"(ptr
// CHECK:   call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func readKeyPath(_ n: node) -> UnsafeMutablePointer<node>? {
  return n[keyPath: \node.parent]
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}writeKeyPath{{.*}}"(ptr
// CHECK:   call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: {{^}$}}
public func writeKeyPath(_ n: inout node, _ p: UnsafeMutablePointer<node>?) {
  n[keyPath: \node.parent] = p
}

// Clang fields have no property descriptor to reference.
public func keyPathParent() -> WritableKeyPath<node, UnsafeMutablePointer<node>?> {
  return \node.parent
}
