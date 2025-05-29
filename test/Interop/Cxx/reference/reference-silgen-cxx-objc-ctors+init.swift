// REQUIRES: objc_interop
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -I %S/Inputs -enable-experimental-cxx-interop -Ounchecked %s | %FileCheck %s

import ConstRefCxxObjCCtorInitParameter

var a: Int32 = 32
var b = IntWrapper(a)
var c = ObjCSwiftBridge(embedded: b)

// FIXME: the const-ref C++ Constructor here is not getting an @in_guaranteed or even an @in convention here.
// CHECK: {{%[0-9]+}} = function_ref @_ZN10IntWrapperC1ERKi : $@convention(c) (@in_guaranteed Int32) -> @out IntWrapper
// CHECK: {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(c) (@in_guaranteed Int32) -> @out IntWrapper
// CHECK: alloc_global @$s4main1cSo15ObjCSwiftBridgeCSgvp
// CHECK: {{%[0-9]+}} = global_addr @$s4main1cSo15ObjCSwiftBridgeCSgvp : $*Optional<ObjCSwiftBridge>
// CHECK: {{%[0-9]+}} = load {{%[0-9]+}} : $*IntWrapper
// CHECK: {{%[0-9]+}} = alloc_ref [objc] $ObjCSwiftBridge
// CHECK: {{%[0-9]+}} = alloc_stack $IntWrapper
// CHECK: store {{%[0-9]+}} to {{%[0-9]+}} : $*IntWrapper
// CHECK: {{%[0-9]+}} = objc_method {{%[0-9]+}} : $ObjCSwiftBridge, #ObjCSwiftBridge.init!initializer.foreign : (ObjCSwiftBridge.Type) -> (IntWrapper) -> ObjCSwiftBridge?, $@convention(objc_method) (@in_guaranteed IntWrapper, @owned ObjCSwiftBridge) -> @owned Optional<ObjCSwiftBridge>
// CHECK: {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(objc_method) (@in_guaranteed IntWrapper, @owned ObjCSwiftBridge) -> @owned Optional<ObjCSwiftBridge>
// CHECK: dealloc_stack {{%[0-9]+}} : $*IntWrapper
