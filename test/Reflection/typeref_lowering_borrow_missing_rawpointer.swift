// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/BorrowMissing.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name BorrowMissing -o %t/%target-library-name(TypesToReflect)
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: no_asan

// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros

// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// Lowering a Borrow needs the type info of Builtin.RawPointer, which comes
// from the standard library's reflection metadata. When it isn't available
// (here, libswiftCore is not passed to the dump tool), lowering must fail
// cleanly instead of dereferencing a null type info.

// Borrow of a type that doesn't itself need any stdlib metadata to lower.
13BorrowMissing1EOBW
// CHECK: (builtin_borrow
// CHECK-NEXT: (enum BorrowMissing.E))
// CHECK-NEXT: Invalid lowering
