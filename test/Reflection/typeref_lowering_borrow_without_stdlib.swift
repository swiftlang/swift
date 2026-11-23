// REQUIRES: no_asan
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift %t/Empty.swift -parse-as-library -emit-library %no-fixup-chains -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)

// Without the standard library there is no type info for Builtin.RawPointer,
// which lowering a Builtin.Borrow depends on.
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) -dump-type-lowering < %t/types.txt | %FileCheck %s

// CHECK:      (builtin_borrow
// CHECK-NEXT:   (struct TypeLowering.Empty))
// CHECK-NEXT: Invalid lowering

//--- types.txt
12TypeLowering5EmptyVBW

//--- Empty.swift
public struct Empty {}
