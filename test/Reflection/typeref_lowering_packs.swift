// REQUIRES: no_asan
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// RUN: %empty-directory(%t)

// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// RUN: %target-build-swift -target %target-swift-5.9-abi-triple %S/Inputs/Packs.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple %S/Inputs/Packs.swift %S/Inputs/main.swift -emit-module -emit-executable %no-fixup-chains -module-name TypeLowering -o %t/TypesToReflect

// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s
// RUN: %target-swift-reflection-dump %t/TypesToReflect %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

12TypeLowering6SimpleV

// CHECK: (struct TypeLowering.Simple)
// CHECK: (struct size=52 alignment=8 stride=56 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:   (field name=x1 offset=0
// CHECK:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:       (field name=t offset=0
// CHECK:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:   (field name=x2 offset=8
// CHECK:     (struct size=12 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:       (field name=t offset=0
// CHECK:         (tuple size=12 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:           (field offset=8
// CHECK:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK:   (field name=x3 offset=24
// CHECK:     (struct size=28 alignment=8 stride=32 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:       (field name=t offset=0
// CHECK:         (tuple size=28 alignment=8 stride=32 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:           (field offset=8
// CHECK:             (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field name=_guts offset=0
// CHECK:                 (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                   (field name=_object offset=0
// CHECK:                     (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                       (field name=_countAndFlagsBits offset=0
// CHECK:                         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                           (field name=_value offset=0
// CHECK:                             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:                       (field name=_object offset=8
// CHECK:                         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))))))))
// CHECK:           (field offset=24
// CHECK:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))))

12TypeLowering7ComplexV

// CHECK: (struct TypeLowering.Complex)
// CHECK: (struct size=368 alignment=8 stride=368 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:   (field name=x1 offset=0
// CHECK:     (struct size=48 alignment=8 stride=48 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:       (field name=tuple1 offset=0
// CHECK:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK:       (field name=tuple2 offset=0
// CHECK:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK:       (field name=tuple3 offset=0
// CHECK:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK:       (field name=tuple4 offset=0
// CHECK:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK:       (field name=func1 offset=0
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func2 offset=16
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func3 offset=32
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=nominal1 offset=48
// CHECK:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:       (field name=nominal2 offset=48
// CHECK:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:       (field name=nominal3 offset=48
// CHECK:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:   (field name=x2 offset=48
// CHECK:     (struct size=112 alignment=8 stride=112 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:       (field name=tuple1 offset=0
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:       (field name=tuple2 offset=8
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=tuple3 offset=24
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:       (field name=tuple4 offset=32
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:       (field name=func1 offset=40
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func2 offset=56
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func3 offset=72
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=nominal1 offset=88
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:       (field name=nominal2 offset=96
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=t offset=0
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=_value offset=0
// CHECK:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK:       (field name=nominal3 offset=104
// CHECK:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))))
// CHECK:   (field name=x3 offset=160
// CHECK:     (struct size=208 alignment=8 stride=208 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:       (field name=tuple1 offset=0
// CHECK:         (tuple size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:           (field offset=8
// CHECK:             (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field name=_guts offset=0
// CHECK:                 (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                   (field name=_object offset=0
// CHECK:                     (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                       (field name=_countAndFlagsBits offset=0
// CHECK:                         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                           (field name=_value offset=0
// CHECK:                             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:                       (field name=_object offset=8
// CHECK:                         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))))))))))
// CHECK:       (field name=tuple2 offset=24
// CHECK:         (tuple size=32 alignment=8 stride=32 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field name=function offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:               (field name=context offset=8
// CHECK:                 (reference kind=strong refcounting=native))))
// CHECK:           (field offset=16
// CHECK:             (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field name=function offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:               (field name=context offset=8
// CHECK:                 (reference kind=strong refcounting=native))))))
// CHECK:       (field name=tuple3 offset=56
// CHECK:         (tuple size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=t offset=0
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=_value offset=0
// CHECK:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:           (field offset=8
// CHECK:             (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field name=t offset=0
// CHECK:                 (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                   (field name=_guts offset=0
// CHECK:                     (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                       (field name=_object offset=0
// CHECK:                         (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                           (field name=_countAndFlagsBits offset=0
// CHECK:                             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                               (field name=_value offset=0
// CHECK:                                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:                           (field name=_object offset=8
// CHECK:                             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))))))))))))
// CHECK:       (field name=tuple4 offset=80
// CHECK:         (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field offset=0
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:           (field offset=8
// CHECK:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field name=_value offset=0
// CHECK:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:       (field name=func1 offset=96
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func2 offset=112
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=func3 offset=128
// CHECK:         (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=function offset=0
// CHECK:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))
// CHECK:           (field name=context offset=8
// CHECK:             (reference kind=strong refcounting=native))))
// CHECK:       (field name=nominal1 offset=144
// CHECK:         (struct size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field offset=0
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=_value offset=0
// CHECK:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:               (field offset=8
// CHECK:                 (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                   (field name=_guts offset=0
// CHECK:                     (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                       (field name=_object offset=0
// CHECK:                         (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                           (field name=_countAndFlagsBits offset=0
// CHECK:                             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                               (field name=_value offset=0
// CHECK:                                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:                           (field name=_object offset=8
// CHECK:                             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))))))))))))
// CHECK:       (field name=nominal2 offset=168
// CHECK:         (struct size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=24 alignment=8 stride=24 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:               (field offset=0
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=t offset=0
// CHECK:                     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                       (field name=_value offset=0
// CHECK:                         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK:               (field offset=8
// CHECK:                 (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                   (field name=t offset=0
// CHECK:                     (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                       (field name=_guts offset=0
// CHECK:                         (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                           (field name=_object offset=0
// CHECK:                             (struct size=16 alignment=8 stride=16 num_extra_inhabitants={{.*}} bitwise_takable=1
// CHECK:                               (field name=_countAndFlagsBits offset=0
// CHECK:                                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                                   (field name=_value offset=0
// CHECK:                                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:                               (field name=_object offset=8
// CHECK:                                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants={{.*}} bitwise_takable=1))))))))))))))
// CHECK:       (field name=nominal3 offset=192
// CHECK:         (struct size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:           (field name=t offset=0
// CHECK:             (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:               (field offset=0
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=_value offset=0
// CHECK:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK:               (field offset=8
// CHECK:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK:                   (field name=_value offset=0
// CHECK:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))))))