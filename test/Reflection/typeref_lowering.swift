// REQUIRES: no_asan
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// XFAIL: OS=windows-msvc
// RUN: %empty-directory(%t)

// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// RUN: %target-build-swift -target %target-swift-5.2-abi-triple -Xfrontend -disable-availability-checking %S/Inputs/TypeLowering.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)
// RUN: %target-build-swift -target %target-swift-5.2-abi-triple -Xfrontend -disable-availability-checking %S/Inputs/TypeLowering.swift %S/Inputs/main.swift -emit-module -emit-executable -module-name TypeLowering -o %t/TypesToReflect

// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// RUN: %target-swift-reflection-dump %t/TypesToReflect %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

12TypeLowering11BasicStructV
// CHECK-64:      (struct TypeLowering.BasicStruct)

// CHECK-64-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=i1 offset=0
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=i2 offset=2
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=i3 offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=bi1 offset=8
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=bi2 offset=10
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=bi3 offset=12
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))

// CHECK-32:      (struct TypeLowering.BasicStruct)
// CHECK-32-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=i1 offset=0
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=i2 offset=2
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=i3 offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=bi1 offset=8
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=bi2 offset=10
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=bi3 offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))

12TypeLowering05AssocA6StructV
// CHECK-64:      (struct TypeLowering.AssocTypeStruct)
// CHECK-64-NEXT: (struct size=36 alignment=2 stride=36 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=t1 offset=0
// CHECK-64-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=b offset=2
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=c offset=4
// CHECK-64-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:           (field offset=2
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64-NEXT:   (field name=t2 offset=8
// CHECK-64-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=b offset=2
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=c offset=4
// CHECK-64-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:           (field offset=2
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64-NEXT:   (field name=t3 offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=b offset=2
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=c offset=4
// CHECK-64-NEXT:         (tuple size=4 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:           (field offset=2
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64-NEXT:   (field name=t4 offset=24
// CHECK-64-NEXT:     (struct size=8 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=b offset=2
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=c offset=4
// CHECK-64-NEXT:         (tuple size=4 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:           (field offset=2
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-64-NEXT:   (field name=t5 offset=32
// CHECK-64-NEXT:     (struct size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=b offset=1
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_value offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:       (field name=c offset=2
// CHECK-64-NEXT:         (tuple size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:           (field offset=1
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                   (field name=_value offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))))))

// CHECK-32:      (struct TypeLowering.AssocTypeStruct)
// CHECK-32-NEXT: (struct size=36 alignment=2 stride=36 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=t1 offset=0
// CHECK-32-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=b offset=2
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=c offset=4
// CHECK-32-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:           (field offset=2
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32-NEXT:   (field name=t2 offset=8
// CHECK-32-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=b offset=2
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=c offset=4
// CHECK-32-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:           (field offset=2
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32-NEXT:   (field name=t3 offset=16
// CHECK-32-NEXT:     (struct size=8 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=b offset=2
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=c offset=4
// CHECK-32-NEXT:         (tuple size=4 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:           (field offset=2
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32-NEXT:   (field name=t4 offset=24
// CHECK-32-NEXT:     (struct size=8 alignment=2 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=b offset=2
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=c offset=4
// CHECK-32-NEXT:         (tuple size=4 alignment=2 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:           (field offset=2
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32-NEXT:   (field name=t5 offset=32
// CHECK-32-NEXT:     (struct size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=b offset=1
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=_value offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:       (field name=c offset=2
// CHECK-32-NEXT:         (tuple size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:           (field offset=1
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                   (field name=_value offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))))))

12TypeLowering3BoxVys5Int16VG_s5Int32Vt
// CHECK-64-NEXT: (tuple
// CHECK-64-NEXT:   (bound_generic_struct TypeLowering.Box
// CHECK-64-NEXT:     (struct Swift.Int16))
// CHECK-64-NEXT:   (struct Swift.Int32))

// CHECK-32-NEXT: (tuple
// CHECK-32-NEXT:   (bound_generic_struct TypeLowering.Box
// CHECK-32-NEXT:     (struct Swift.Int16))
// CHECK-32-NEXT:   (struct Swift.Int32))

// CHECK-64-NEXT: (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field offset=0
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32-NEXT: (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field offset=0
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

12TypeLowering15ReferenceStructV
// CHECK-64:      (struct TypeLowering.ReferenceStruct)
// CHECK-64-NEXT: (struct size=48 alignment=8 stride=48 num_extra_inhabitants=[[PTR_XI:2048|4096|2147483647]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=strongRef offset=0
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field name=optionalStrongRef offset=8
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1:2047|4095|2147483646]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=strongRefTuple offset=16
// CHECK-64-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:   (field name=optionalStrongRefTuple offset=32
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=native))
// CHECK-64-NEXT:           (field offset=8
// CHECK-64-NEXT:             (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:       (case name=none index=1))))

// CHECK-32: (struct TypeLowering.ReferenceStruct)
// CHECK-32-NEXT: (struct size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=strongRef offset=0
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field name=optionalStrongRef offset=4
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=strongRefTuple offset=8
// CHECK-32-NEXT:     (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:   (field name=optionalStrongRefTuple offset=16
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=native))
// CHECK-32-NEXT:           (field offset=4
// CHECK-32-NEXT:             (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:       (case name=none index=1))))

12TypeLowering22UnownedReferenceStructV
// CHECK-64:      (struct TypeLowering.UnownedReferenceStruct)
// CHECK-64-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=unownedRef offset=0
// CHECK-64-NEXT:     (reference kind=unowned refcounting=native)))

// CHECK-32:      (struct TypeLowering.UnownedReferenceStruct)
// CHECK-32-NEXT: (struct size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=unownedRef offset=0
// CHECK-32-NEXT:     (reference kind=unowned refcounting=native)))

12TypeLowering19WeakReferenceStructV
// CHECK-64:      (struct TypeLowering.WeakReferenceStruct)
// CHECK-64-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:   (field name=weakRef offset=0
// CHECK-64-NEXT:     (reference kind=weak refcounting=native)))

// CHECK-32:      (struct TypeLowering.WeakReferenceStruct)
// CHECK-32-NEXT: (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:   (field name=weakRef offset=0
// CHECK-32-NEXT:     (reference kind=weak refcounting=native)))

12TypeLowering24UnmanagedReferenceStructV
// CHECK-64:      (struct TypeLowering.UnmanagedReferenceStruct)
// CHECK-64-NEXT: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=unmanagedRef offset=0
// CHECK-64-NEXT:     (reference kind=unmanaged refcounting=native)))

// CHECK-32:      (struct TypeLowering.UnmanagedReferenceStruct)
// CHECK-32-NEXT: (struct size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=unmanagedRef offset=0
// CHECK-32-NEXT:     (reference kind=unmanaged refcounting=native)))

12TypeLowering14FunctionStructV
// CHECK-64:      (struct TypeLowering.FunctionStruct)
// CHECK-64-NEXT: (struct size=64 alignment=8 stride=64 num_extra_inhabitants=[[PTR_XI_2:4096|2147483647]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=thickFunction offset=0
// CHECK-64-NEXT:     (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=function offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=context offset=8
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:   (field name=optionalThickFunction offset=16
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_2_SUB_1:4095|2147483646]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=function offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:           (field name=context offset=8
// CHECK-64-NEXT:             (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=thinFunction offset=32
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:   (field name=optionalThinFunction offset=40
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=cFunction offset=48
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:   (field name=optionalCFunction offset=56
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_2]] bitwise_takable=1))
// CHECK-64-NEXT:       (case name=none index=1))))

// CHECK-32: (struct TypeLowering.FunctionStruct)
// CHECK-32-NEXT: (struct size=32 alignment=4 stride=32 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=thickFunction offset=0
// CHECK-32-NEXT:     (thick_function size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=function offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=context offset=4
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:   (field name=optionalThickFunction offset=8
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (thick_function size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=function offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=context offset=4
// CHECK-32-NEXT:             (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=thinFunction offset=16
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:   (field name=optionalThinFunction offset=20
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=cFunction offset=24
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:   (field name=optionalCFunction offset=28
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (case name=none index=1))))

12TypeLowering17ExistentialStructV
// CHECK-64:      (struct TypeLowering.ExistentialStruct)
// CHECK-64-NEXT: (struct size=416 alignment=8 stride=416 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=any offset=0
// CHECK-64-NEXT:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAny offset=32
// CHECK-64-NEXT:     (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=24
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyObject offset=64
// CHECK-64-NEXT:     (class_existential size=8 alignment=8 stride=8
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-64-NEXT:   (field name=optionalAnyObject offset=72
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (class_existential size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=object offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyProto offset=80
// CHECK-64-NEXT:     (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyProto offset=120
// CHECK-64-NEXT:     (single_payload_enum size=40 alignment=8 stride=40 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=24
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=32
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyProtoComposition offset=160
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyProtoComposition offset=208
// CHECK-64-NEXT:     (single_payload_enum size=48 alignment=8 stride=48 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=24
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=32
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=40
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyClassBoundProto1 offset=256
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyClassBoundProto1 offset=272
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=object offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyClassBoundProto2 offset=288
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyClassBoundProto2 offset=304
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=object offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyClassBoundProtoComposition1 offset=320
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyClassBoundProtoComposition1 offset=336
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=object offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyClassBoundProtoComposition2 offset=352
// CHECK-64-NEXT:     (class_existential size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyClassBoundProtoComposition2 offset=376
// CHECK-64-NEXT:     (single_payload_enum size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (class_existential size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=object offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=16
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=classConstrainedP1 offset=400
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))

// CHECK-32: (struct TypeLowering.ExistentialStruct)
// CHECK-32-NEXT: (struct size=208 alignment=4 stride=208 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=any offset=0
// CHECK-32-NEXT:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAny offset=16
// CHECK-32-NEXT:     (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=12
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyObject offset=32
// CHECK-32-NEXT:     (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:   (field name=optionalAnyObject offset=36
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=object offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyProto offset=40
// CHECK-32-NEXT:     (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyProto offset=60
// CHECK-32-NEXT:     (single_payload_enum size=20 alignment=4 stride=20 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=12
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=16
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyProtoComposition offset=80
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyProtoComposition offset=104
// CHECK-32-NEXT:     (single_payload_enum size=24 alignment=4 stride=24 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=12
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=16
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=20
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyClassBoundProto1 offset=128
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyClassBoundProto1 offset=136
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=object offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyClassBoundProto2 offset=144
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyClassBoundProto2 offset=152
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=object offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyClassBoundProtoComposition1 offset=160
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyClassBoundProtoComposition1 offset=168
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=object offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyClassBoundProtoComposition2 offset=176
// CHECK-32-NEXT:     (class_existential size=12 alignment=4 stride=12 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyClassBoundProtoComposition2 offset=188
// CHECK-32-NEXT:     (single_payload_enum size=12 alignment=4 stride=12 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (class_existential size=12 alignment=4 stride=12 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=object offset=0
// CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=8
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=classConstrainedP1 offset=200
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

12TypeLowering24UnownedExistentialStructV
// CHECK-64:      (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=0
// CHECK-64-NEXT:   (field name=unownedRef offset=0
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=unowned refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))

// CHECK-32:      (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:   (field name=unownedRef offset=0
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=unowned refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

12TypeLowering30UnownedNativeExistentialStructV
// CHECK-64:      (struct TypeLowering.UnownedNativeExistentialStruct)
// CHECK-64-NEXT: (struct size=48 alignment=8 stride=48 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=unownedRef1 offset=0
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=unownedRef2 offset=16
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=unownedRef3 offset=32
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))

// CHECK-32:      (struct TypeLowering.UnownedNativeExistentialStruct)
// CHECK-32-NEXT: (struct size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=unownedRef1 offset=0
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=unownedRef2 offset=8
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=unownedRef3 offset=16
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=unowned refcounting=native))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

12TypeLowering21WeakExistentialStructV
// CHECK-64-NEXT: (struct TypeLowering.WeakExistentialStruct)
// CHECK-64-NEXT: (struct size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=0
// CHECK-64-NEXT:   (field name=weakAnyObject offset=0
// CHECK-64-NEXT:     (class_existential size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=weak refcounting=unknown))))
// CHECK-64-NEXT:   (field name=weakAnyClassBoundProto offset=8
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=weak refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))

// CHECK-32-NEXT: (struct TypeLowering.WeakExistentialStruct)
// CHECK-32-NEXT: (struct size=12 alignment=4 stride=12 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:   (field name=weakAnyObject offset=0
// CHECK-32-NEXT:     (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=weak refcounting=unknown))))
// CHECK-32-NEXT:   (field name=weakAnyClassBoundProto offset=4
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=weak refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

12TypeLowering26UnmanagedExistentialStructV
// CHECK-64:      (struct TypeLowering.UnmanagedExistentialStruct)
// CHECK-64-NEXT: (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=unmanagedRef offset=0
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=unmanaged refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))

// CHECK-32:      (struct TypeLowering.UnmanagedExistentialStruct)
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=unmanagedRef offset=0
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=unmanaged refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

12TypeLowering14MetatypeStructV
// CHECK-64:      (struct TypeLowering.MetatypeStruct)
// CHECK-64-NEXT: (struct size=152 alignment=8 stride=152 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=any offset=0
// CHECK-64-NEXT:     (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAny offset=8
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyObject offset=16
// CHECK-64-NEXT:     (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyObject offset=24
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyProto offset=32
// CHECK-64-NEXT:     (existential_metatype size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyProto offset=48
// CHECK-64-NEXT:     (single_payload_enum size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (existential_metatype size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=anyProtoComposition offset=64
// CHECK-64-NEXT:     (existential_metatype size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=optionalAnyProtoComposition offset=88
// CHECK-64-NEXT:     (single_payload_enum size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (existential_metatype size=24 alignment=8 stride=24 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:           (field name=metadata offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=8
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:           (field name=wtable offset=16
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=structMetatype offset=112
// CHECK-64-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:   (field name=optionalStructMetatype offset=112
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=classMetatype offset=120
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:   (field name=optionalClassMetatype offset=128
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=abstractMetatype offset=136
// CHECK-64-NEXT:     (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=t offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=u offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1)))))

// CHECK-32: (struct TypeLowering.MetatypeStruct)
// CHECK-32-NEXT: (struct size=76 alignment=4 stride=76 num_extra_inhabitants=4096
// CHECK-32-NEXT:   (field name=any offset=0
// CHECK-32-NEXT:     (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAny offset=4
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyObject offset=8
// CHECK-32-NEXT:     (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyObject offset=12
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyProto offset=16
// CHECK-32-NEXT:     (existential_metatype size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyProto offset=24
// CHECK-32-NEXT:     (single_payload_enum size=8 alignment=4 stride=8 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (existential_metatype size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=anyProtoComposition offset=32
// CHECK-32-NEXT:     (existential_metatype size=12 alignment=4 stride=12 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=optionalAnyProtoComposition offset=44
// CHECK-32-NEXT:     (single_payload_enum size=12 alignment=4 stride=12 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (existential_metatype size=12 alignment=4 stride=12 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:           (field name=metadata offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=4
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:           (field name=wtable offset=8
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=structMetatype offset=56
// CHECK-32-NEXT:     (builtin size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32-NEXT:   (field name=optionalStructMetatype offset=56
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=classMetatype offset=60
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:   (field name=optionalClassMetatype offset=64
// CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32-NEXT:       (case name=some index=0 offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (case name=none index=1)))
// CHECK-32-NEXT:   (field name=abstractMetatype offset=68
// CHECK-32-NEXT:     (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=t offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=u offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1)))))

12TypeLowering10EnumStructV
// CHECK-64: (struct TypeLowering.EnumStruct)
// CHECK-64-NEXT: (struct size=81 alignment=8 stride=88 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=empty offset=0
// CHECK-64-NEXT:     (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:   (field name=noPayload offset=0
// CHECK-64-NEXT:     (no_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
// CHECK-64-NEXT:       (case name=A index=0)
// CHECK-64-NEXT:       (case name=B index=1)
// CHECK-64-NEXT:       (case name=C index=2)
// CHECK-64-NEXT:       (case name=D index=3)))
// CHECK-64-NEXT:   (field name=sillyNoPayload offset=1
// CHECK-64-NEXT:    (multi_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:      (case name=A index=0 offset=0
// CHECK-64-NEXT:        (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:      (case name=B index=1 offset=0
// CHECK-64-NEXT:        (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:      (case name=C index=2)
// CHECK-64-NEXT:      (case name=D index=3)))
// CHECK-64-NEXT:   (field name=singleton offset=8
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field name=singlePayload offset=16
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=Indirect index=0 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=Nothing index=1)))
// CHECK-64-NEXT:   (field name=multiPayloadConcrete offset=24
// CHECK-64-NEXT:     (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants={{(2045|125)}} bitwise_takable=1
// CHECK-64-NEXT:       (case name=Left index=0 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=Right index=1 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=Donkey index=2)
// CHECK-64-NEXT:       (case name=Mule index=3)
// CHECK-64-NEXT:       (case name=Horse index=4)))
// CHECK-64-NEXT:   (field name=multiPayloadGenericFixed offset=32
// CHECK-64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-64-NEXT:       (case name=Left index=0 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=Right index=1 offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))
// CHECK-64-NEXT:       (case name=Donkey index=2)
// CHECK-64-NEXT:       (case name=Mule index=3)
// CHECK-64-NEXT:       (case name=Horse index=4)))
// CHECK-64-NEXT:   (field name=multiPayloadGenericDynamic offset=48
// CHECK-64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-64-NEXT:       (case name=Left index=0 offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=Right index=1 offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (case name=Donkey index=2)
// CHECK-64-NEXT:       (case name=Mule index=3)
// CHECK-64-NEXT:       (case name=Horse index=4)))
// CHECK-64-NEXT:   (field name=optionalOptionalRef offset=64
// CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_2:2147483645|2046|4094]] bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI_SUB_1]] bitwise_takable=1
// CHECK-64-NEXT:           (case name=some index=0 offset=0
// CHECK-64-NEXT:             (reference kind=strong refcounting=native))
// CHECK-64-NEXT:           (case name=none index=1)))
// CHECK-64-NEXT:       (case name=none index=1)))
// CHECK-64-NEXT:   (field name=optionalOptionalPtr offset=72
// CHECK-64-NEXT:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (case name=some index=0 offset=0
// CHECK-64-NEXT:         (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (case name=some index=0 offset=0
// CHECK-64-NEXT:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1
// CHECK-64-NEXT:               (field name=_rawValue offset=0
// CHECK-64-NEXT:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:           (case name=none index=1)))
// CHECK-64-NEXT:       (case name=none index=1))))

12TypeLowering23EnumStructWithOwnershipV
// CHECK-64:      (struct TypeLowering.EnumStructWithOwnership)
// CHECK-64-NEXT: (struct size=25 alignment=8 stride=32 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-64-NEXT:   (field name=multiPayloadConcrete offset=0
// CHECK-64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-64-NEXT:       (case name=Left index=0 offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:           (field name=weakRef offset=0
// CHECK-64-NEXT:             (reference kind=weak refcounting=native))))
// CHECK-64-NEXT:       (case name=Right index=1 offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:           (field name=weakRef offset=0
// CHECK-64-NEXT:             (reference kind=weak refcounting=native))))))
// CHECK-64-NEXT:   (field name=multiPayloadGeneric offset=16
// CHECK-64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-64-NEXT:       (case name=Left index=0 offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:           (field name=weakRef offset=0
// CHECK-64-NEXT:             (reference kind=weak refcounting=native))))
// CHECK-64-NEXT:       (case name=Right index=1 offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))

// CHECK-32:      (struct TypeLowering.EnumStructWithOwnership)
// CHECK-32-NEXT: (struct size=13 alignment=4 stride=16 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-32-NEXT:   (field name=multiPayloadConcrete offset=0
// CHECK-32-NEXT:     (multi_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-32-NEXT:       (case name=Left index=0 offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:           (field name=weakRef offset=0
// CHECK-32-NEXT:             (reference kind=weak refcounting=native))))
// CHECK-32-NEXT:       (case name=Right index=1 offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:           (field name=weakRef offset=0
// CHECK-32-NEXT:             (reference kind=weak refcounting=native))))))
// CHECK-32-NEXT:   (field name=multiPayloadGeneric offset=8
// CHECK-32-NEXT:     (multi_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=254 bitwise_takable=0
// CHECK-32-NEXT:       (case name=Left index=0 offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:           (field name=weakRef offset=0
// CHECK-32-NEXT:             (reference kind=weak refcounting=native))))
// CHECK-32-NEXT:       (case name=Right index=1 offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))))

Bo
// CHECK-64:      (builtin Builtin.NativeObject)
// CHECK-64-NEXT: (reference kind=strong refcounting=native)

// CHECK-32:      (builtin Builtin.NativeObject)
// CHECK-32-NEXT: (reference kind=strong refcounting=native)

BO
// CHECK-64:      (builtin Builtin.UnknownObject)
// CHECK-64-NEXT: (reference kind=strong refcounting=unknown)

// CHECK-32:      (builtin Builtin.UnknownObject)
// CHECK-32-NEXT: (reference kind=strong refcounting=unknown)

12TypeLowering22RefersToOtherAssocTypeV
// CHECK-64:      (struct TypeLowering.RefersToOtherAssocType)
// CHECK-64-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=y offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32:      (struct TypeLowering.RefersToOtherAssocType)
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=y offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

12TypeLowering18GenericOnAssocTypeVyAA13OpaqueWitnessVG
// CHECK-64:      (bound_generic_struct TypeLowering.GenericOnAssocType
// CHECK-64-NEXT:   (struct TypeLowering.OpaqueWitness))
// CHECK-64-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=y offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32:      (bound_generic_struct TypeLowering.GenericOnAssocType
// CHECK-32-NEXT:   (struct TypeLowering.OpaqueWitness))
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=y offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

BD
// CHECK-64:      (builtin Builtin.DefaultActorStorage)
// CHECK-64-NEXT:    (builtin size=96 alignment=16 stride=96 num_extra_inhabitants=0 bitwise_takable=1)

// CHECK-32:      (builtin Builtin.DefaultActorStorage)
// CHECK-32-NEXT:    (builtin size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1)
