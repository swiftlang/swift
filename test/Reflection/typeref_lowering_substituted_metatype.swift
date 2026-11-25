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
// RUN: %target-build-swift %S/Inputs/TypeLoweringSubstitutedMetatype.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s

// Substituting a generic argument keeps the empty representation of a metatype
// whose instance type isn't a type parameter, like Box<T>.Type and [T].Type.
// Only T.Type and the class metatype Klass<T>.Type take up storage.

12TypeLowering23HasSubstitutedMetatypesVySiG
// CHECK:      (bound_generic_struct TypeLowering.HasSubstitutedMetatypes
// CHECK-NEXT:   (struct Swift.Int))
// CHECK-NEXT: (struct size=24 alignment=8 stride=24 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-NEXT:   (field name=box offset=0
// CHECK-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-NEXT:   (field name=array offset=0
// CHECK-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-NEXT:   (field name=param offset=0
// CHECK-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1))
// CHECK-NEXT:   (field name=klass offset=8
// CHECK-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1))
// CHECK-NEXT:   (field name=x offset=16
