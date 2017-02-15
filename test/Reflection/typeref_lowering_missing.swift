// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/Missing.swift -parse-as-library -emit-module -emit-library -module-name TypeLowering -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect -binary-filename %platform-module-dir/libswiftCore.dylib -dump-type-lowering < %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64

V12TypeLowering11BasicStruct
// CHECK: (struct TypeLowering.BasicStruct)
// CHECK: Invalid lowering

GSqV12TypeLowering11BasicStruct_
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct TypeLowering.BasicStruct))
// CHECK: Invalid lowering

GV12TypeLowering3BarV12TypeLowering11BasicStruct_
// CHECK: (bound_generic_struct TypeLowering.Bar
// CHECK:   (struct TypeLowering.BasicStruct))
// CHECK: Invalid lowering
