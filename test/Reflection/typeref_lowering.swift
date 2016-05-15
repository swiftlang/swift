// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/TypeLowering.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension -binary-filename %platform-module-dir/libswiftCore.%target-dylib-extension -dump-type-lowering < %s | FileCheck %s --check-prefix=CHECK-%target-ptrsize

V12TypeLowering11BasicStruct
// CHECK-64:      (struct TypeLowering.BasicStruct)

// CHECK-64-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=i1 offset=0
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))
// CHECK-64-NEXT:   (field name=i2 offset=2
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-64-NEXT:   (field name=i3 offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-64-NEXT:   (field name=bi1 offset=8
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:   (field name=bi2 offset=10
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:   (field name=bi3 offset=12
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))))

// CHECK-32: (struct TypeLowering.BasicStruct)
// CHECK-32-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=i1 offset=0
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))
// CHECK-32-NEXT:   (field name=i2 offset=2
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-32-NEXT:   (field name=i3 offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32-NEXT:   (field name=bi1 offset=8
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:   (field name=bi2 offset=10
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:   (field name=bi3 offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))))

V12TypeLowering15AssocTypeStruct
// CHECK-64:      (struct TypeLowering.AssocTypeStruct)
// CHECK-64-NEXT: (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=t offset=0
// CHECK-64-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=a offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field offset=0
// CHECK-64-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:       (field name=b offset=2
// CHECK-64-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field name=value offset=0
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field offset=0
// CHECK-64-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:       (field name=c offset=4
// CHECK-64-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:                   (field offset=0
// CHECK-64-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:           (field offset=2
// CHECK-64-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:               (field name=value offset=0
// CHECK-64-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64-NEXT:                   (field offset=0
// CHECK-64-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))))))))

// CHECK-32:     (struct TypeLowering.AssocTypeStruct)
// CHECK-32-NEXT: (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=t offset=0
// CHECK-32-NEXT:     (struct size=7 alignment=2 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=a offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field offset=0
// CHECK-32-NEXT:                 (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:       (field name=b offset=2
// CHECK-32-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field name=value offset=0
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field offset=0
// CHECK-32-NEXT:                 (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:       (field name=c offset=4
// CHECK-32-NEXT:         (tuple size=3 alignment=2 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:                   (field offset=0
// CHECK-32-NEXT:                     (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:           (field offset=2
// CHECK-32-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:               (field name=value offset=0
// CHECK-32-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32-NEXT:                   (field offset=0
// CHECK-32-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))))))))

TGV12TypeLowering3BoxVs5Int16_Vs5Int32_
// CHECK-64-NEXT: (tuple
// CHECK-64-NEXT:   (bound_generic_struct TypeLowering.Box
// CHECK-64-NEXT:     (struct Swift.Int16))
// CHECK-64-NEXT:   (struct Swift.Int32))

// CHECK-32-NEXT: (tuple
// CHECK-32-NEXT:   (bound_generic_struct TypeLowering.Box
// CHECK-32-NEXT:     (struct Swift.Int16))
// CHECK-32-NEXT:   (struct Swift.Int32))

// CHECK-64-NEXT: (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=0
// CHECK-64-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64-NEXT:           (field offset=0
// CHECK-64-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-64-NEXT:   (field offset=4
// CHECK-64-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))

// CHECK-32-NEXT: (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=0
// CHECK-32-NEXT:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32-NEXT:           (field offset=0
// CHECK-32-NEXT:             (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))))
// CHECK-32-NEXT:   (field offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))


V12TypeLowering15ReferenceStruct
// CHECK-64:      (struct TypeLowering.ReferenceStruct)
// CHECK-64-NEXT: (struct size=40 alignment=8 stride=40 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=strongRef offset=0
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field name=strongOptionalRef offset=8
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field name=unownedRef offset=16
// CHECK-64-NEXT:     (reference kind=unowned refcounting=native))
// CHECK-64-NEXT:   (field name=weakRef offset=24
// CHECK-64-NEXT:     (reference kind=weak refcounting=native))
// CHECK-64-NEXT:   (field name=unmanagedRef offset=32
// CHECK-64-NEXT:     (reference kind=unmanaged refcounting=native)))

// CHECK-32: (struct TypeLowering.ReferenceStruct)
// CHECK-32-NEXT: (struct size=20 alignment=4 stride=20 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=strongRef offset=0
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field name=strongOptionalRef offset=4
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field name=unownedRef offset=8
// CHECK-32-NEXT:     (reference kind=unowned refcounting=native))
// CHECK-32-NEXT:   (field name=weakRef offset=12
// CHECK-32-NEXT:     (reference kind=weak refcounting=native))
// CHECK-32-NEXT:   (field name=unmanagedRef offset=16
// CHECK-32-NEXT:     (reference kind=unmanaged refcounting=native)))

V12TypeLowering14FunctionStruct
// CHECK-64:      (struct TypeLowering.FunctionStruct)
// CHECK-64-NEXT: (struct size=32 alignment=8 stride=32 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=thickFunction offset=0
// CHECK-64-NEXT:     (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=function offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=context offset=8
// CHECK-64-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-64-NEXT:   (field name=thinFunction offset=16
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:   (field name=cFunction offset=24
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1)))

// CHECK-32: (struct TypeLowering.FunctionStruct)
// CHECK-32-NEXT: (struct size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=thickFunction offset=0
// CHECK-32-NEXT:     (thick_function size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=function offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=context offset=4
// CHECK-32-NEXT:         (reference kind=strong refcounting=native))))
// CHECK-32-NEXT:   (field name=thinFunction offset=8
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:   (field name=cFunction offset=12
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1)))

V12TypeLowering17ExistentialStruct
// CHECK-64:      (struct TypeLowering.ExistentialStruct)
// CHECK-64-NEXT: (struct size=224 alignment=8 stride=224 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=any offset=0
// CHECK-64-NEXT:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyObject offset=32
// CHECK-64-NEXT:     (class_existential size=8 alignment=8 stride=8
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-64-NEXT:   (field name=anyProto offset=40
// CHECK-64-NEXT:     (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyProtoComposition offset=80
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=value offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyClassBoundProto1 offset=128
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyClassBoundProto2 offset=144
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyClassBoundProtoComposition1 offset=160
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyClassBoundProtoComposition2 offset=176
// CHECK-64-NEXT:     (class_existential size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=weakAnyObject offset=200
// CHECK-64-NEXT:     (class_existential size=8 alignment=8 stride=8
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=weak refcounting=unknown))))
// CHECK-64-NEXT:   (field name=weakAnyClassBoundProto offset=208
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=weak refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1)))))

// CHECK-32: (struct TypeLowering.ExistentialStruct)
// CHECK-32-NEXT: (struct size=112 alignment=4 stride=112 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=any offset=0
// CHECK-32-NEXT:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyObject offset=16
// CHECK-32-NEXT:     (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:   (field name=anyProto offset=20
// CHECK-32-NEXT:     (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyProtoComposition offset=40
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=value offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyClassBoundProto1 offset=64
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyClassBoundProto2 offset=72
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyClassBoundProtoComposition1 offset=80
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyClassBoundProtoComposition2 offset=88
// CHECK-32-NEXT:     (class_existential size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=weakAnyObject offset=100
// CHECK-32-NEXT:     (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=weak refcounting=unknown))))
// CHECK-32-NEXT:   (field name=weakAnyClassBoundProto offset=104
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=weak refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1)))))

V12TypeLowering14MetatypeStruct
// CHECK-64:      (struct TypeLowering.MetatypeStruct)
// CHECK-64-NEXT: (struct size=80 alignment=8 stride=80 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=any offset=0
// CHECK-64-NEXT:     (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyObject offset=8
// CHECK-64-NEXT:     (existential_metatype size=8 alignment=8 stride=8 num_extra_inhabitants=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyProto offset=16
// CHECK-64-NEXT:     (existential_metatype size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=anyProtoComposition offset=32
// CHECK-64-NEXT:     (existential_metatype size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=wtable offset=16
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64-NEXT:   (field name=structMetatype offset=56
// CHECK-64-NEXT:     (builtin size=0 alignment=1 stride=0 num_extra_inhabitants=0))
// CHECK-64-NEXT:   (field name=classMetatype offset=56
// CHECK-64-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:   (field name=abstractMetatype offset=64
// CHECK-64-NEXT:     (struct size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=t offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64-NEXT:       (field name=u offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1)))))

// CHECK-32: (struct TypeLowering.MetatypeStruct)
// CHECK-32-NEXT: (struct size=40 alignment=4 stride=40 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=any offset=0
// CHECK-32-NEXT:     (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyObject offset=4
// CHECK-32-NEXT:     (existential_metatype size=4 alignment=4 stride=4 num_extra_inhabitants=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyProto offset=8
// CHECK-32-NEXT:     (existential_metatype size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=anyProtoComposition offset=16
// CHECK-32-NEXT:     (existential_metatype size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=wtable offset=8
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32-NEXT:   (field name=structMetatype offset=28
// CHECK-32-NEXT:     (builtin size=0 alignment=1 stride=0 num_extra_inhabitants=0))
// CHECK-32-NEXT:   (field name=classMetatype offset=28
// CHECK-32-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:   (field name=abstractMetatype offset=32
// CHECK-32-NEXT:     (struct size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=t offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32-NEXT:       (field name=u offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1)))))

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
