// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ImportedTypes.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -I %S/Inputs -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect -binary-filename %platform-module-dir/libswiftCore.dylib -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64

V12TypeLowering9HasCTypes
// CHECK:     (struct TypeLowering.HasCTypes)
// CHECK-NEXT: (struct size=44 alignment=8 stride=48 num_extra_inhabitants=0
// CHECK-NEXT:   (field name=mcs offset=0
// CHECK-NEXT:     (builtin size=24 alignment=8 stride=24 num_extra_inhabitants=0))
// CHECK-NEXT:   (field name=mce offset=24
// CHECK-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))
// CHECK-NEXT:   (field name=mcu offset=32
// CHECK-NEXT:     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))
// CHECK-NEXT:   (field name=mcsbf offset=40
// CHECK-NEXT:     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))

