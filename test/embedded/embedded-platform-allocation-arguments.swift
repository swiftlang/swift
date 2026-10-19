// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded %t/main.swift -c -o %t/main.o
// RUN: %target-clang -x c -c %t/platform.c -o %t/platform.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o %t/platform.o %embedded-stdlib -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation %t/main.swift -c -o %t/main-typed.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main-typed.o %t/platform.o %embedded-stdlib -o %t/a-typed.out
// RUN: %target-run %t/a-typed.out | %FileCheck %s --check-prefix=TYPED

// REQUIRES: OS=macosx
// REQUIRES: SWIFT_STDLIB_ARCH=arm64
// REQUIRES: executable_test
// REQUIRES: embedded_stdlib
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation
// REQUIRES: swift_embedded_platform
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=xros

// swift_slowDealloc passes the arguments of _swift_deallocate in the order
// declared in EmbeddedPlatform.h.
// CHECK: allocate alignment=[[ALIGN:[0-9]+]] size=[[SIZE:[0-9]+]]{{$}}
// CHECK-NEXT: deallocate alignment=[[ALIGN]] size=[[SIZE]]{{$}}

// The typed allocation functions receive an alignment rather than an alignment
// mask.
// TYPED: typedAllocate alignment=8{{$}}
// TYPED-NEXT: typedDeallocate alignment=8{{$}}
// UnsafeMutablePointer passes the default alignment, 0.
// TYPED-NEXT: typedAllocate alignment=0{{$}}
// TYPED-NEXT: typedDeallocate alignment=0{{$}}

//--- main.swift
final class C {}
var c: C?

@inline(never) func test() {
  c = C()
  c = nil
  UnsafeMutablePointer<Int>.allocate(capacity: 1).deallocate()
}
test()

//--- platform.c
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void *_swift_allocate(size_t alignment, size_t size, uint64_t flags) {
  printf("allocate alignment=%zu size=%zu\n", alignment, size);
  void *p = NULL;
  return posix_memalign(&p, alignment, size) == 0 ? p : NULL;
}

void _swift_deallocate(void *ptr, size_t alignment, size_t size,
                       uint64_t flags) {
  printf("deallocate alignment=%zu size=%zu\n", alignment, size);
  free(ptr);
}

void *_swift_typedAllocate(size_t size, size_t alignment, uint64_t flags,
                           uint64_t typeId) {
  printf("typedAllocate alignment=%zu\n", alignment);
  void *p = NULL;
  return posix_memalign(&p, alignment ? alignment : 16, size) == 0 ? p : NULL;
}

void _swift_typedDeallocate(void *ptr, size_t size, size_t alignment,
                            uint64_t flags, uint64_t typeId) {
  printf("typedDeallocate alignment=%zu\n", alignment);
  free(ptr);
}
