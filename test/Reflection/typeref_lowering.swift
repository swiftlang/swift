// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/TypeLowering.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -Xfrontend -enable-reflection-metadata -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect -binary-filename %platform-module-dir/libswiftCore.dylib -dump-type-lowering < %s | FileCheck %s

// REQUIRES: OS=macosx

V12TypeLowering11BasicStruct
// CHECK:      (struct TypeLowering.BasicStruct)
// CHECK-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-NEXT:   (field name=i1 offset=0
// CHECK-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=_value offset=0
// CHECK-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))
// CHECK-NEXT:   (field name=i2 offset=2
// CHECK-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=_value offset=0
// CHECK-NEXT:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-NEXT:   (field name=i3 offset=4
// CHECK-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=_value offset=0
// CHECK-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-NEXT:   (field name=bi1 offset=8
// CHECK-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=value offset=0
// CHECK-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=_value offset=0
// CHECK-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-NEXT:   (field name=bi2 offset=10
// CHECK-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=value offset=0
// CHECK-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=_value offset=0
// CHECK-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-NEXT:   (field name=bi3 offset=12
// CHECK-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=value offset=0
// CHECK-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=_value offset=0
// CHECK-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))))

V12TypeLowering15AssocTypeStruct
// CHECK:      (struct TypeLowering.AssocTypeStruct)
// CHECK-NEXT: (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-NEXT:   (field name=t offset=0
// CHECK-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=a offset=0
// CHECK-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=value offset=0
// CHECK-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:               (field name=_value offset=0
// CHECK-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-NEXT:       (field name=b offset=2
// CHECK-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=value offset=0
// CHECK-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:               (field name=_value offset=0
// CHECK-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-NEXT:       (field name=c offset=4
// CHECK-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0
// CHECK-NEXT:           (field name= offset=0
// CHECK-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:               (field name=value offset=0
// CHECK-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:                   (field name=_value offset=0
// CHECK-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-NEXT:           (field name= offset=2
// CHECK-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:               (field name=value offset=0
// CHECK-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-NEXT:                   (field name=_value offset=0
// CHECK-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))))))))

TGV12TypeLowering3BoxVs5Int16_Vs5Int32_
// CHECK-NEXT: (tuple
// CHECK-NEXT:   (bound_generic_struct TypeLowering.Box
// CHECK-NEXT:     (struct Swift.Int16))
// CHECK-NEXT:   (struct Swift.Int32))

// CHECK-NEXT: (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-NEXT:   (field name= offset=0
// CHECK-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=value offset=0
// CHECK-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-NEXT:           (field name=_value offset=0
// CHECK-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-NEXT:   (field name= offset=4
// CHECK-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-NEXT:       (field name=_value offset=0
// CHECK-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))
