// RUN: %target-swift-frontend -emit-silgen %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

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
// CHECK:         [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK:         strong_retain [[PROJECTION]]
// CHECK:         apply {{%.*}}<@opened{{.*}}>([[PROJECTION]])
// CHECK:         strong_release %0
// CHECK-NOT:     strong_release
func getIntPropExistential(_ a: ProtocolA) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21setIntPropExistentialFPS_9ProtocolA_T_
// CHECK:         [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK:         strong_retain [[PROJECTION]]
// CHECK:         apply {{%.*}}<@opened{{.*}}>({{%.*}}, [[PROJECTION]])
// CHECK:         strong_release %0
// CHECK_NOT:     strong_release
func setIntPropExistential(_ a: ProtocolA) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGeneric
// CHECK-NOT:     strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
func getIntPropGeneric<T: ProtocolA>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17setIntPropGeneric
// CHECK-NOT:     strong_retain %0
// CHECK:         apply {{%.*}}<T>({{%.*}}, %0)
// CHECK:         strong_release %0
func setIntPropGeneric<T: ProtocolA>(_ a: T) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolB_Si
// CHECK:         [[PROJECTION:%.*]] = open_existential_addr %0
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $@opened({{".*"}}) ProtocolB
// CHECK:         copy_addr [[PROJECTION]] to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
// CHECK:         destroy_addr %0
func getIntPropExistential(_ a: ProtocolB) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGeneric
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK:         copy_addr %0 to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}<T>([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
// CHECK:         destroy_addr %0
func getIntPropGeneric<T: ProtocolB>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21getIntPropExistentialFPS_9ProtocolO_Si
// CHECK:         debug_value
// CHECK-NEXT:    [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK-NEXT:    strong_retain [[PROJECTION]]
// CHECK-NEXT:    [[METHOD:%.*]] = witness_method
// CHECK-NEXT:    apply [[METHOD]]<@opened{{.*}}>([[PROJECTION]])
// CHECK-NEXT:    strong_release [[PROJECTION]]
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    return
func getIntPropExistential(_ a: ProtocolO) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime21setIntPropExistentialFPS_9ProtocolO_T_
// CHECK:         [[PROJECTION:%.*]] = open_existential_ref %0
// CHECK-NEXT:    strong_retain [[PROJECTION]]
// CHECK:    [[METHOD:%.*]] = witness_method
// CHECK-NEXT:    apply [[METHOD]]<@opened{{.*}}>({{.*}}, [[PROJECTION]])
// CHECK-NEXT:    strong_release [[PROJECTION]]
// CHECK-NEXT:    strong_release %0
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func setIntPropExistential(_ a: ProtocolO) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17getIntPropGeneric
// CHECK-NOT:     strong_retain %0
// CHECK:         apply {{%.*}}<T>(%0)
// CHECK:         strong_release %0
// CHECK-NOT:     strong_release %0
func getIntPropGeneric<T: ProtocolO>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_TF30generic_property_base_lifetime17setIntPropGeneric
// CHECK-NOT:     strong_retain %0
// CHECK:         apply {{%.*}}<T>({{%.*}}, %0)
// CHECK:         strong_release %0
// CHECK-NOT:     strong_release %0
func setIntPropGeneric<T: ProtocolO>(_ a: T) {
  a.intProp = 0
}
