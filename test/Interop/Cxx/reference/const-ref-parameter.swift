// REQUIRES: objc_interop

import ConstRefParameter

func testFunction() {
    let a = OptionsStruct(intOption: 1, floatOption: 2.0)

    let objc = OptionsConsumerObjC(options: a)
    _ = objc.doOtherThing(withOptions: a)
    _ = OptionsConsumerObjC.consumer(withOptions: a)
    _ = OptionsConsumerObjC.doThing(withOptions: a)

    var cxx = OptionsConsumerCxx(a)
    _ = cxx.doOtherThing(a)
    _ = OptionsConsumerCxx.build(a)
    _ = OptionsConsumerCxx.doThing(a)
}

// RUN: %target-swift-ide-test -print-module -module-to-print=ConstRefParameter -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s

// CHECK-IDE-TEST: class OptionsConsumerObjC
// CHECK-IDE-TEST: init(options: OptionsStruct)
// COM: FIXME: should it be consumer(options:)?
// CHECK-IDE-TEST: class func consumer(withOptions options: OptionsStruct) -> Self
// COM: FIXME: should it be doThing(options:)?
// CHECK-IDE-TEST: class func doThing(withOptions options: OptionsStruct) -> Int32
// COM: FIXME: should it be doOtherThing(options:)?
// CHECK-IDE-TEST: func doOtherThing(withOptions options: OptionsStruct) -> Float

// CHECK-IDE-TEST: struct OptionsConsumerCxx
// CHECK-IDE-TEST: init(_ options: OptionsStruct)
// CHECK-IDE-TEST: static func build(_ options: OptionsStruct) -> OptionsConsumerCxx
// CHECK-IDE-TEST: static func doThing(_ options: OptionsStruct) -> Int32
// CHECK-IDE-TEST: mutating func doOtherThing(_ options: OptionsStruct) -> Float


// RUN: %target-swift-frontend -c -enable-experimental-cxx-interop -enable-objc-interop -I %S/Inputs %s -Xllvm -sil-print-types -emit-silgen -o - | %FileCheck %s

// CHECK: [[FN1:%[0-9]+]] = function_ref @$sSo19OptionsConsumerObjCC7optionsABSo0A6StructV_tcfC : $@convention(method) (OptionsStruct, @thick OptionsConsumerObjC.Type) -> @owned OptionsConsumerObjC
// CHECK-NEXT: apply [[FN1]]
// CHECK-SAME: : $@convention(method) (OptionsStruct, @thick OptionsConsumerObjC.Type) -> @owned OptionsConsumerObjC

// CHECK: [[FN2:%[0-9]+]] = objc_method {{%[0-9]+}} : $OptionsConsumerObjC, #OptionsConsumerObjC.doOtherThing!foreign : (OptionsConsumerObjC) -> (OptionsStruct) -> Float, $@convention(objc_method) (@in_guaranteed OptionsStruct, OptionsConsumerObjC) -> Float
// CHECK-NEXT: apply [[FN2]]
// CHECK-SAME: : $@convention(objc_method) (@in_guaranteed OptionsStruct, OptionsConsumerObjC) -> Float

// CHECK: [[FN3:%[0-9]+]] = objc_method {{%[0-9]+}} : $@objc_metatype OptionsConsumerObjC.Type, #OptionsConsumerObjC.consumer!foreign : (OptionsConsumerObjC.Type) -> (OptionsStruct) -> @dynamic_self OptionsConsumerObjC, $@convention(objc_method) (@in_guaranteed OptionsStruct, @objc_metatype OptionsConsumerObjC.Type) -> @autoreleased OptionsConsumerObjC
// CHECK-NEXT: apply [[FN3]]
// CHECK-SAME: : $@convention(objc_method) (@in_guaranteed OptionsStruct, @objc_metatype OptionsConsumerObjC.Type) -> @autoreleased OptionsConsumerObjC

// CHECK: [[FN4:%[0-9]+]] = objc_method {{%[0-9]+}} : $@objc_metatype OptionsConsumerObjC.Type, #OptionsConsumerObjC.doThing!foreign : (OptionsConsumerObjC.Type) -> (OptionsStruct) -> Int32, $@convention(objc_method) (@in_guaranteed OptionsStruct, @objc_metatype OptionsConsumerObjC.Type) -> Int32
// CHECK-NEXT: apply [[FN4]]
// CHECK-SAME: : $@convention(objc_method) (@in_guaranteed OptionsStruct, @objc_metatype OptionsConsumerObjC.Type) -> Int32

// CHECK: [[FN5:%[0-9]+]] = function_ref @_ZN18OptionsConsumerCxxC1ERK13OptionsStruct : $@convention(c) (@in_guaranteed OptionsStruct) -> @out OptionsConsumerCxx
// CHECK-NEXT: apply [[FN5]]
// CHECK-SAME: : $@convention(c) (@in_guaranteed OptionsStruct) -> @out OptionsConsumerCxx

// CHECK: [[FN6:%[0-9]+]] = function_ref @_ZN18OptionsConsumerCxx12doOtherThingERK13OptionsStruct : $@convention(cxx_method) (@in_guaranteed OptionsStruct, @inout OptionsConsumerCxx) -> Float
// CHECK-NEXT: apply [[FN6]]
// CHECK-SAME: : $@convention(cxx_method) (@in_guaranteed OptionsStruct, @inout OptionsConsumerCxx) -> Float

// CHECK: [[FN6:%[0-9]+]] = function_ref @_ZN18OptionsConsumerCxx5buildERK13OptionsStruct : $@convention(c) (@in_guaranteed OptionsStruct) -> OptionsConsumerCxx
// CHECK-NEXT: apply [[FN6]]
// CHECK-SAME: : $@convention(c) (@in_guaranteed OptionsStruct) -> OptionsConsumerCxx

// CHECK: [[FN7:%[0-9]+]] = function_ref @_ZN18OptionsConsumerCxx7doThingERK13OptionsStruct : $@convention(c) (@in_guaranteed OptionsStruct) -> Int32
// CHECK-NEXT: apply [[FN7]]
// CHECK-SAME: : $@convention(c) (@in_guaranteed OptionsStruct) -> Int32
