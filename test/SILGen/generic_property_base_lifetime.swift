// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

protocol ProtocolA: class {
    var intProp: Int { get set }
}

protocol ProtocolB {
    var intProp: Int { get }
}

@objc protocol ProtocolO: class {
    var intProp: Int { get set }
}


// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime21getIntPropExistentialSiAA9ProtocolA_pF : $@convention(thin) (@owned ProtocolA) -> Int {
// CHECK: bb0([[ARG:%.*]] : $ProtocolA):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[BORROWED_ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[WITNESS_METHOD:%.*]] = witness_method $@opened({{.*}}) ProtocolA, #ProtocolA.intProp!getter.1 : {{.*}}, [[PROJECTION]]
// CHECK:   [[RESULT:%.*]] = apply [[WITNESS_METHOD]]<@opened{{.*}}>([[PROJECTION_COPY]])
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '_T030generic_property_base_lifetime21getIntPropExistentialSiAA9ProtocolA_pF'
func getIntPropExistential(_ a: ProtocolA) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime21setIntPropExistentialyAA9ProtocolA_pF : $@convention(thin) (@owned ProtocolA) -> () {
// CHECK: bb0([[ARG:%.*]] : $ProtocolA):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[BORROWED_ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[WITNESS_METHOD:%.*]] = witness_method $@opened({{.*}}) ProtocolA, #ProtocolA.intProp!setter.1 : {{.*}}, [[PROJECTION]]
// CHECK:   apply [[WITNESS_METHOD]]<@opened{{.*}}>({{%.*}}, [[PROJECTION_COPY]])
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_T030generic_property_base_lifetime21setIntPropExistentialyAA9ProtocolA_pF'
func setIntPropExistential(_ a: ProtocolA) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:    apply {{%.*}}<T>([[BORROWED_ARG]])
// CHECK:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:    destroy_value [[ARG]]
func getIntPropGeneric<T: ProtocolA>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime17setIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   apply {{%.*}}<T>({{%.*}}, [[BORROWED_ARG]])
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
func setIntPropGeneric<T: ProtocolA>(_ a: T) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime21getIntPropExistentialSiAA9ProtocolB_pF
// CHECK:         [[PROJECTION:%.*]] = open_existential_addr immutable_access %0
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $@opened({{".*"}}) ProtocolB
// CHECK:         copy_addr [[PROJECTION]] to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
// CHECK:         destroy_addr %0
func getIntPropExistential(_ a: ProtocolB) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK:         copy_addr %0 to [initialization] [[STACK]]
// CHECK:         apply {{%.*}}<T>([[STACK]])
// CHECK:         destroy_addr [[STACK]]
// CHECK:         dealloc_stack [[STACK]]
// CHECK:         destroy_addr %0
func getIntPropGeneric<T: ProtocolB>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime21getIntPropExistentialSiAA9ProtocolO_pF : $@convention(thin) (@owned ProtocolO) -> Int {
// CHECK: bb0([[ARG:%.*]] : $ProtocolO):
// CHECK:  [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:  [[PROJECTION:%.*]] = open_existential_ref [[BORROWED_ARG]]
// CHECK:  [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:  [[METHOD:%.*]] = witness_method [volatile] $@opened({{.*}}) ProtocolO, #ProtocolO.intProp!getter.1.foreign : {{.*}}, [[PROJECTION]]
// CHECK:  apply [[METHOD]]<@opened{{.*}}>([[PROJECTION_COPY]])
// CHECK:  destroy_value [[PROJECTION_COPY]]
// CHECK:  end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:  destroy_value [[ARG]]
// CHECK: } // end sil function '_T030generic_property_base_lifetime21getIntPropExistentialSiAA9ProtocolO_pF'
func getIntPropExistential(_ a: ProtocolO) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime21setIntPropExistentialyAA9ProtocolO_pF : $@convention(thin) (@owned ProtocolO) -> () {
// CHECK: bb0([[ARG:%.*]] : $ProtocolO):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[PROJECTION:%.*]] = open_existential_ref [[BORROWED_ARG]]
// CHECK:   [[PROJECTION_COPY:%.*]] = copy_value [[PROJECTION]]
// CHECK:   [[METHOD:%.*]] = witness_method [volatile] $@opened({{.*}}) ProtocolO, #ProtocolO.intProp!setter.1.foreign : {{.*}}, [[PROJECTION]]
// CHECK:   apply [[METHOD]]<@opened{{.*}}>({{.*}}, [[PROJECTION_COPY]])
// CHECK:   destroy_value [[PROJECTION_COPY]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_T030generic_property_base_lifetime21setIntPropExistentialyAA9ProtocolO_pF'
func setIntPropExistential(_ a: ProtocolO) {
  a.intProp = 0
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime17getIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   apply {{%.*}}<T>([[BORROWED_ARG]])
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
func getIntPropGeneric<T: ProtocolO>(_ a: T) -> Int {
  return a.intProp
}

// CHECK-LABEL: sil hidden @_T030generic_property_base_lifetime17setIntPropGeneric{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : $T):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   apply {{%.*}}<T>({{%.*}}, [[BORROWED_ARG]])
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
func setIntPropGeneric<T: ProtocolO>(_ a: T) {
  a.intProp = 0
}
