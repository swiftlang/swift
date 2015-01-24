// RUN: %target-swift-frontend -emit-silgen %s -disable-objc-attr-requires-foundation-module | FileCheck %s

protocol ProtocolA: class {
    var intProp: Int { get set }
}

protocol ProtocolB {
    var intProp: Int { get }
}

@objc protocol ProtocolO: class {
    var intProp: Int { get set }
}


// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolA_Si
// CHECK:         strong_retain %0
// CHECK:         [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK:         apply {{%.*}}<@opened{{.*}}>([[PROJECTION]])
// CHECK:         strong_release %0
func getIntPropExistential(a: ProtocolA) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21setIntPropExistentialFPS_9ProtocolA_T_
// CHECK:         strong_retain %0
// CHECK:         [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK:         apply {{%.*}}<@opened{{.*}}>({{%.*}}, [[PROJECTION]])
// CHECK:         strong_release %0
func setIntPropExistential(a: ProtocolA) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolA__FQ_Si
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
func getIntPropGeneric<T: ProtocolA>(a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17setIntPropGenericUS_9ProtocolA__FQ_T_
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>({{%.*}}, %0)
// CHECK:         strong_release %0
func setIntPropGeneric<T: ProtocolA>(a: T) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolB_Si
// CHECK:         [[PROJECTION:%.*]] = open_existential %0
// CHECK:         apply {{%.*}}([[PROJECTION]])
// CHECK:         destroy_addr %0
func getIntPropExistential(a: ProtocolB) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolB__FQ_Si
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         destroy_addr %0
func getIntPropGeneric<T: ProtocolB>(a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolO_Si
// CHECK:         debug_value
// CHECK-NEXT:    strong_retain %0
// CHECK-NEXT:    [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK-NEXT:    [[METHOD:%.*]] = witness_method
// CHECK-NEXT:    apply [[METHOD]]<@opened{{.*}}>([[PROJECTION]])
// CHECK-NEXT:    strong_release [[PROJECTION]]
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    return
func getIntPropExistential(a: ProtocolO) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21setIntPropExistentialFPS_9ProtocolO_T_
// CHECK:         strong_retain %0
// CHECK-NEXT:    [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK-NEXT:    [[METHOD:%.*]] = witness_method
// CHECK-NEXT:    apply [[METHOD]]<@opened{{.*}}>({{.*}}, [[PROJECTION]])
// CHECK-NEXT:    strong_release [[PROJECTION]]
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func setIntPropExistential(a: ProtocolO) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGenericUS_9ProtocolO__FQ_Si
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
// CHECK:         strong_release %0
func getIntPropGeneric<T: ProtocolO>(a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17setIntPropGenericUS_9ProtocolO__FQ_T_
// CHECK:         strong_retain %0
// CHECK:         apply {{%.*}}<T>({{%.*}}, %0)
// CHECK:         strong_release %0
// CHECK:         strong_release %0
func setIntPropGeneric<T: ProtocolO>(a: T) {
  a.intProp = 0
}
