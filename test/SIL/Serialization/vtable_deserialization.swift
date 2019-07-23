// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/vtable_deserialization_input.swift -emit-module-path %t/vtable_deserialization_input.swiftmodule -emit-module
// RUN: %target-swift-frontend %s -emit-sil -I %t | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-sil -O -I %t | %FileCheck %s --check-prefix=OPT

import vtable_deserialization_input

// Make sure we devirtualized the call and inlined the method body.
// CHECK: function_ref @$s28vtable_deserialization_input5ClassC11firstMethodyyFZ
// OPT: function_ref @$s28vtable_deserialization_input7unknownyyF
Class.firstMethod()


// The methods should not be deserialized in the mandatory pipeline.

// CHECK-LABEL: sil [serialized] @$s28vtable_deserialization_input5ClassC11firstMethodyyFZ : $@convention(method) (@thick Class.Type) -> (){{$}}
// OPT-LABEL: sil public_external @$s28vtable_deserialization_input5ClassC11firstMethodyyFZ : $@convention(method) (@thick Class.Type) -> () {

// CHECK-LABEL: sil [serialized] @$s28vtable_deserialization_input5ClassC12secondMethodyyFZ : $@convention(method) (@thick Class.Type) -> (){{$}}
// OPT-LABEL: sil public_external @$s28vtable_deserialization_input5ClassC12secondMethodyyFZ : $@convention(method) (@thick Class.Type) -> () {

// CHECK-LABEL: sil [serialized] @$s28vtable_deserialization_input5ClassC11thirdMethodyyFZ : $@convention(method) (@thick Class.Type) -> (){{$}}
// OPT-LABEL: sil public_external @$s28vtable_deserialization_input5ClassC11thirdMethodyyFZ : $@convention(method) (@thick Class.Type) -> () {

// Make sure we deserialized the vtable.

// CHECK:      sil_vtable [serialized] Class {
// CHECK-NEXT:   #Class.firstMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC11firstMethodyyFZ
// CHECK-NEXT:   #Class.secondMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC12secondMethodyyFZ
// CHECK-NEXT:   #Class.thirdMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC11thirdMethodyyFZ
// CHECK-NEXT:   #Class.deinit!deallocator.1: @$s28vtable_deserialization_input5ClassCfD
// CHECK-NEXT: }

// OPT:      sil_vtable Class {
// OPT-NEXT:   #Class.firstMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC11firstMethodyyFZ
// OPT-NEXT:   #Class.secondMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC12secondMethodyyFZ
// OPT-NEXT:   #Class.thirdMethod!1: (Class.Type) -> () -> () : @$s28vtable_deserialization_input5ClassC11thirdMethodyyFZ
// OPT-NEXT:   #Class.deinit!deallocator.1: @$s28vtable_deserialization_input5ClassCfD
// OPT-NEXT: }
