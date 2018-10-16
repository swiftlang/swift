
// RUN: %target-swift-emit-silgen -module-name generic_property_base_lifetime %s -disable-objc-attr-requires-foundation-module -enable-objc-interop -enable-sil-ownership | %FileCheck %s

protocol ProtocolA: class {
    var intProp: Int { get set }
}

protocol ProtocolB {
    var intProp: Int { get }
}

@objc protocol ProtocolO: class {
    var intProp: Int { get set }
}


// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime21getIntPropExistentialySiAA9ProtocolA_pF : $@convention(thin) (@guaranteed ProtocolA) -> Int {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $ProtocolA):
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[BORROWED_PROJECTION_COPY:%.*]] = begin_borrow [[PROJECTION_COPY]]
// CHECK:   [[WITNESS_METHOD:%.*]] = witness_method $@opened({{.*}}) ProtocolA, #ProtocolA.intProp!getter.1 : {{.*}}, [[PROJECTION]]
// CHECK:   [[RESULT:%.*]] = apply [[WITNESS_METHOD]]<@opened{{.*}}>([[BORROWED_PROJECTION_COPY]])
// CHECK:   end_borrow [[BORROWED_PROJECTION_COPY]] from [[PROJECTION_COPY]]
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$S30generic_property_base_lifetime21getIntPropExistentialySiAA9ProtocolA_pF'
func getIntPropExistential(_ a: ProtocolA) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime21setIntPropExistentialyyAA9ProtocolA_pF : $@convention(thin) (@guaranteed ProtocolA) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $ProtocolA):
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[BORROWED_PROJECTION_COPY:%.*]] = begin_borrow [[PROJECTION_COPY]]
// CHECK:   [[WITNESS_METHOD:%.*]] = witness_method $@opened({{.*}}) ProtocolA, #ProtocolA.intProp!setter.1 : {{.*}}, [[PROJECTION]]
// CHECK:   apply [[WITNESS_METHOD]]<@opened{{.*}}>({{%.*}}, [[BORROWED_PROJECTION_COPY]])
// CHECK:   end_borrow [[BORROWED_PROJECTION_COPY]] from [[PROJECTION_COPY]]
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK: } // end sil function '$S30generic_property_base_lifetime21setIntPropExistentialyyAA9ProtocolA_pF'
func setIntPropExistential(_ a: ProtocolA) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:    apply {{%.*}}<T>([[ARG]])
// CHECK: } // end sil function '$S30generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F'
func getIntPropGeneric<T: ProtocolA>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime17setIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   apply {{%.*}}<T>({{%.*}}, [[ARG]])
func setIntPropGeneric<T: ProtocolA>(_ a: T) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime21getIntPropExistentialySiAA9ProtocolB_pF
// CHECK:         [[PROJECTION:%.*]] = open_existential_addr immutable_access %0
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $@opened({{".*"}}) ProtocolB
// CHECK:         copy_addr [[PROJECTION]] to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
func getIntPropExistential(_ a: ProtocolB) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK:         copy_addr %0 to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}<T>([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
func getIntPropGeneric<T: ProtocolB>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime21getIntPropExistentialySiAA9ProtocolO_pF : $@convention(thin) (@guaranteed ProtocolO) -> Int {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $ProtocolO):
// CHECK:  [[PROJECTION:%.*]] = open_existential_ref [[ARG]]
// CHECK:  [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:  [[PROJECTION_BORROW:%.*]] = begin_borrow [[PROJECTION_COPY]]
// CHECK:  [[METHOD:%.*]] = objc_method [[PROJECTION_BORROW]] : $@opened({{.*}}) ProtocolO, #ProtocolO.intProp!getter.1.foreign : {{.*}}
// CHECK:  apply [[METHOD]]<@opened{{.*}}>([[PROJECTION_BORROW]])
// CHECK:  destroy_value [[PROJECTION_COPY]]
// CHECK: } // end sil function '$S30generic_property_base_lifetime21getIntPropExistentialySiAA9ProtocolO_pF'
func getIntPropExistential(_ a: ProtocolO) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime21setIntPropExistentialyyAA9ProtocolO_pF : $@convention(thin) (@guaranteed ProtocolO) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $ProtocolO):
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[METHOD:%.*]] = objc_method [[PROJECTION_COPY]] : $@opened({{.*}}) ProtocolO, #ProtocolO.intProp!setter.1.foreign : {{.*}}
// CHECK:   apply [[METHOD]]<@opened{{.*}}>({{.*}}, [[PROJECTION_COPY]])
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK: } // end sil function '$S30generic_property_base_lifetime21setIntPropExistentialyyAA9ProtocolO_pF'
func setIntPropExistential(_ a: ProtocolO) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   apply {{%.*}}<T>([[ARG]])
func getIntPropGeneric<T: ProtocolO>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @$S30generic_property_base_lifetime17setIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @guaranteed $T):
// CHECK:   apply {{%.*}}<T>({{%.*}}, [[ARG]])
func setIntPropGeneric<T: ProtocolO>(_ a: T) {
  a.intProp = 0
}
