// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/TypeLoweringObjectiveC.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect -binary-filename %platform-module-dir/libswiftCore.dylib -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64

12TypeLowering14FunctionStructV
// CHECK:      (struct TypeLowering.FunctionStruct)
// CHECK-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-NEXT:   (field name=blockFunction offset=0
// CHECK-NEXT:     (reference kind=strong refcounting=unknown)))

12TypeLowering14HasObjCClassesC
// CHECK: (class TypeLowering.HasObjCClasses)
// CHECK-NEXT: (reference kind=strong refcounting=native)

12TypeLowering16NSObjectSubclassC
// CHECK: (class TypeLowering.NSObjectSubclass)
// CHECK-NEXT: (reference kind=strong refcounting=unknown)

12TypeLowering11HasObjCEnumV
// CHECK: (struct size=24 alignment=8 stride=24 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-NEXT:   (field name=optionalEnum offset=0
// CHECK-NEXT:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:       (field name=some offset=0
// CHECK-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-NEXT:   (field name=reference offset=16
// CHECK-NEXT:     (class_existential size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-NEXT:       (field name=object offset=0
// CHECK-NEXT:         (reference kind=strong refcounting=unknown)))))

12TypeLowering22UnownedReferenceStructV
// CHECK-64:      (struct TypeLowering.UnownedReferenceStruct)
// CHECK-64-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=0
// CHECK-64-NEXT:   (field name=unownedRef offset=0
// CHECK-64-NEXT:     (reference kind=unowned refcounting=unknown)))

// CHECK-32:      (struct TypeLowering.UnownedReferenceStruct)
// CHECK-32-NEXT: (struct size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:   (field name=unownedRef offset=0
// CHECK-32-NEXT:     (reference kind=unowned refcounting=unknown)))
