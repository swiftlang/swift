// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/cmodules/testModA.m -c -o %t/testModA.o
// RUN: %target-build-swift -target %target-swift-5.2-abi-triple -Xfrontend -disable-availability-checking %S/Inputs/TypeLoweringObjCClassMetatype.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -I %S/Inputs/cmodules -o %t/%target-library-name(TypesToReflect) %t/testModA.o
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

12TypeLowering20HasObjCClassMetatypeV
// CHECK:      (struct TypeLowering.HasObjCClassMetatype)
// CHECK-NEXT: (struct size={{[0-9]+}} alignment={{[0-9]+}} stride={{[0-9]+}} num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-NEXT:   (field name=metatype offset=0
// CHECK-NEXT:     (builtin size={{4|8}} alignment={{4|8}} stride={{4|8}} num_extra_inhabitants={{[0-9]+}} bitwise_takable=1)))
// CHECK-NOT: Invalid lowering
