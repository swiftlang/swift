// REQUIRES: no_asan
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// rdar://100558042
// UNSUPPORTED: CPU=arm64e
//
// rdar://188082318
// XFAIL: OS=linux-android
// XFAIL: OS=linux-androideabi
//
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/TypeLoweringTupleMetatype.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s

// The metatype of a tuple is empty, even if an element is a class.

12TypeLowering16HasTupleMetatypeV
// CHECK:      (struct TypeLowering.HasTupleMetatype)
// CHECK-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:   (field name=tuple offset=0
// CHECK-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-NEXT:   (field name=x offset=0

12TypeLowering23HasGenericTupleMetatypeVyAA1CCG
// CHECK:      (bound_generic_struct TypeLowering.HasGenericTupleMetatype
// CHECK-NEXT:   (class TypeLowering.C))
// CHECK-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:   (field name=tuple offset=0
// CHECK-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-NEXT:   (field name=x offset=0
