
// RUN: %target-swift-emit-silgen -module-name protocol_optional -parse-as-library -disable-objc-attr-requires-foundation-module -enable-objc-interop -enable-sil-ownership %s | %FileCheck %s

@objc protocol P1 {
  @objc optional func method(_ x: Int)

  @objc optional var prop: Int { get }

  @objc optional subscript (i: Int) -> Int { get }
}

// CHECK-LABEL: sil hidden @$S17protocol_optional0B13MethodGeneric1tyx_tAA2P1RzlF : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalMethodGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TBOX]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed (Int) -> ()> }
  // CHECK:   project_box [[OPT_BOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   alloc_stack $Optional<@callee_guaranteed (Int) -> ()>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.method!1.foreign
  var methodRef = t.method
}
// CHECK: } // end sil function '$S17protocol_optional0B13MethodGeneric1tyx_tAA2P1RzlF'

// CHECK-LABEL: sil hidden @$S17protocol_optional0B15PropertyGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalPropertyGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TBOX]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<Int> }
  // CHECK:   project_box [[OPT_BOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.prop!getter.1.foreign
  var propertyRef = t.prop
}
// CHECK: } // end sil function '$S17protocol_optional0B15PropertyGeneric{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden @$S17protocol_optional0B16SubscriptGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalSubscriptGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TBOX]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<Int> }
  // CHECK:   project_box [[OPT_BOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   [[INT64:%[0-9]+]] = metatype $@thin Int.Type
  // CHECK:   [[FIVELIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 5
  // CHECK:   [[INTCONV:%[0-9]+]] = function_ref @$SSi2{{[_0-9a-zA-Z]*}}fC
  // CHECK:   [[FIVE:%[0-9]+]] = apply [[INTCONV]]([[FIVELIT]], [[INT64]]) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int
  // CHECK:   alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.subscript!getter.1.foreign
  var subscriptRef = t[5]
}
// CHECK: } // end sil function '$S17protocol_optional0B16SubscriptGeneric{{[_0-9a-zA-Z]*}}F'
