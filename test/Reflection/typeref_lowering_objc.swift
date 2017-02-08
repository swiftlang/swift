// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/TypeLoweringObjectiveC.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect -binary-filename %platform-module-dir/libswiftCore.dylib -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64

12TypeLowering14FunctionStructV
// CHECK:      (struct TypeLowering.FunctionStruct)
// CHECK-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647
// CHECK-NEXT:   (field name=blockFunction offset=0
// CHECK-NEXT:     (reference kind=strong refcounting=unknown)))

12TypeLowering14HasObjCClassesC
// CHECK: (class TypeLowering.HasObjCClasses)
// CHECK-NEXT: (reference kind=strong refcounting=native)

12TypeLowering16NSObjectSubclassC
// CHECK: (class TypeLowering.NSObjectSubclass)
// CHECK-NEXT: (reference kind=strong refcounting=unknown)

