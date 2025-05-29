// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_dynamic_init.h %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

protocol Hoozit {
    init()
}

protocol Wotsit {
    init()
}

class Gadget: NSObject, Hoozit {
    required override init() {
        super.init()
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s17objc_dynamic_init6GadgetCACycfC : $@convention(method) (@thick Gadget.Type) -> @owned Gadget
// CHECK: [[OBJC_METATYPE:%.*]] = thick_to_objc_metatype %0 : $@thick Gadget.Type to $@objc_metatype Gadget.Type
// CHECK: [[SELF:%.*]] = alloc_ref_dynamic [objc] %1 : $@objc_metatype Gadget.Type, $Gadget
// CHECK: [[INIT:%.*]] = function_ref @$s17objc_dynamic_init6GadgetCACycfcTD : $@convention(method) (@owned Gadget) -> @owned Gadget
// CHECK: [[NEW_SELF:%.*]] = apply [[INIT]]([[SELF]]) : $@convention(method) (@owned Gadget) -> @owned Gadget
// CHECK: return [[NEW_SELF]]

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17objc_dynamic_init6GadgetCAA6HoozitA2aDPxycfCTW :
// CHECK:         function_ref @$s17objc_dynamic_init6GadgetCACycfC

class Gizmo: Gadget, Wotsit {
    required init() {
        super.init()
    }
}

class Thingamabob: ObjCBaseWithInitProto {
    required init(proto: Int) {
        super.init(proto: proto)
    }
}

final class Bobamathing: Thingamabob {
    required init(proto: Int) {
        super.init(proto: proto)
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s17objc_dynamic_init8callInityyF : $@convention(thin) () -> ()
// CHECK: [[METATYPE:%.*]] = metatype $@thick Gadget.Type
// CHECK: [[CTOR:%.*]] = function_ref @$s17objc_dynamic_init6GadgetCACycfC
// CHECK: [[INSTANCE:%.*]] = apply [[CTOR]]([[METATYPE]])
// CHECK: destroy_value [[INSTANCE]]

func callInit() {
    let metatype = Gadget.self
    _ = metatype.init()
}

// CHECK-LABEL: sil_vtable Gadget {
// CHECK-NOT:     #Gadget.init!allocator

// CHECK-LABEL: sil_vtable Gizmo {
// CHECK-NOT:     #Gadget.init!allocator
