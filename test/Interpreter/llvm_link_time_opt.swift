// UNSUPPORTED: OS=windows-msvc
// static library is not well supported yet on Windows

// UNSUPPORTED: OS=xros

// UNSUPPORTED: OS=linux-android, OS=linux-androideabi
// The Android NDK ships its own lld, older than the in-tree LLVM that builds
// the compiler. During LTO it rejects an IR attribute value the newer LLVM
// emits: "invalid value for 'frame-pointer' attribute: non-leaf-no-reserve".
// This is a linker/LLVM version mismatch, not a bug in this test. Once we
// have a new enough lld in the NDK, re-enable this.
// See: oss-swift-rebranch-package-swift-sdk-for-android build 219.

// For LTO, the linker dlopen()'s the libLTO library, which is a scenario that
// ASan cannot work in ("Interceptors are not working, AddressSanitizer is
// loaded too late").
// REQUIRES: no_asan

// UNSUPPORTED: linker_overridden

// RUN: %empty-directory(%t)
// RUN: %use_just_built_liblto %target-swiftc_driver -emit-library -static -lto=llvm-full %lto_flags -emit-module %S/Inputs/lto/module1.swift -working-directory %t
// RUN: %use_just_built_liblto %target-swiftc_driver -lto=llvm-full %lto_flags %s -I%t -L%t -lmodule1 -module-name main -o %t/main
// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s

// CHECK-NOT: _$s7module120unusedPublicFunctionyyF

import module1
