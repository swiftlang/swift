
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name protocol_optional -parse-as-library -disable-objc-attr-requires-foundation-module -enable-objc-interop %s | %FileCheck %s

@objc protocol P1 {
  @objc optional func method(_ x: Int)
  @objc optional func methodReturnsSelf() -> Self

  @objc optional static func staticMethod(_ x: Int)

  @objc optional var prop: Int { get }

  @objc optional subscript (i: Int) -> Int { get }
}

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B13MethodGeneric1tyx_tAA2P1RzlF : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalMethodGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[TLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[TBOX]]
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TLIFETIME]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed (Int) -> ()> }
  // CHECK:   [[OPT_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OPT_BOX]]
  // CHECK:   project_box [[OPT_LIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   alloc_stack $Optional<@callee_guaranteed (Int) -> ()>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.method!foreign
  var methodRef = t.method
}
// CHECK: } // end sil function '$s17protocol_optional0B13MethodGeneric1tyx_tAA2P1RzlF'

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B22StaticMethodRefGeneric1tyx_tAA2P1RzlF : $@convention(thin) <T where T : P1> (@guaranteed T) -> () {
func optionalStaticMethodRefGeneric<T: P1>(t: T) {
  // CHECK: bb0(%0 : @guaranteed $T):
  // CHECK:   [[BOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed (Int) -> ()> }, var
  // CHECK:   [[BOX_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[BOX]]
  // CHECK:   [[UNBOX:%[0-9]+]] = project_box [[BOX_LIFETIME]]
  // CHECK:   [[META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:   [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[META]]
  // CHECK:   alloc_stack $Optional<@callee_guaranteed (Int) -> ()>
  // CHECK:   dynamic_method_br [[OBJC_META]] : $@objc_metatype T.Type, #P1.staticMethod!foreign, bb1, bb2
  var methodRef = T.self.staticMethod
}
// CHECK: } // end sil function '$s17protocol_optional0B22StaticMethodRefGeneric1tyx_tAA2P1RzlF'

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B23MethodUnboundRefGeneric1tyx_tAA2P1RzlF : $@convention(thin) <T where T : P1> (@guaranteed T) -> () {
// CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
// CHECK:   [[BOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @owned Optional<@callee_guaranteed (Int) -> ()> for <τ_0_0> } <T>
// CHECK:   [[BOX_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:   [[PBOX:%[0-9]+]] = project_box [[BOX_LIFETIME]]
// CHECK:   [[THUNK:%[0-9]+]] = function_ref @$[[THUNK_NAME:[_0-9a-zA-Z]+]]
// CHECK:   [[SPEC_THUNK:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<T>()
// CHECK:   [[SUBST_THUNK:%[0-9]+]] = convert_function [[SPEC_THUNK]]
// CHECK:   store [[SUBST_THUNK]] to [init] [[PBOX]]
// CHECK: } // end sil function '$s17protocol_optional0B23MethodUnboundRefGeneric1tyx_tAA2P1RzlF'

// CHECK: sil private [ossa] @$[[THUNK_NAME]] : $@convention(thin) <T where T : P1> (@guaranteed T) -> @owned Optional<@callee_guaranteed (Int) -> ()> {
// CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
// CHECK:   [[T_COPY:%[0-9]+]] = copy_value [[T]]
// CHECK:   alloc_stack $Optional<@callee_guaranteed (Int) -> ()>
// CHECK:   dynamic_method_br [[T_COPY]] : $T, #P1.method!foreign, bb1, bb2
// CHECK: } // end sil function '$[[THUNK_NAME]]'
func optionalMethodUnboundRefGeneric<T : P1>(t: T) {
  var methodRef = T.method
}

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B27MethodUnboundRefExistentialyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX:%[0-9]+]] = alloc_box ${ var @callee_guaranteed (@guaranteed any P1) -> @owned Optional<@callee_guaranteed () -> @owned any P1> }
// CHECK:   [[BOX_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:   [[PBOX:%[0-9]+]] = project_box [[BOX_LIFETIME]]
// CHECK:   [[THUNK:%[0-9]+]] = function_ref @$[[THUNK_NAME:[_0-9a-zA-Z]+]]
// CHECK:   [[THUNK_THICK:%[0-9]+]] = thin_to_thick_function [[THUNK]]
// CHECK:   store [[THUNK_THICK]] to [init] [[PBOX]]
// CHECK:   end_borrow [[BOX_LIFETIME]]
// CHECK: } // end sil function '$s17protocol_optional0B27MethodUnboundRefExistentialyyF'

// CHECK: sil private [ossa] @$[[THUNK_NAME]] : $@convention(thin) (@guaranteed any P1) -> @owned Optional<@callee_guaranteed () -> @owned any P1> {
// CHECK: bb0([[EXIST:%[0-9]+]] : @guaranteed $any P1):
// CHECK:   [[OPENED:%[0-9]+]] = open_existential_ref [[EXIST]] : $any P1 to $[[OPENED_TY:@opened\("[-A-F0-9]+", any P1\) Self]]
// CHECK:   [[OPENED_COPY:%[0-9]+]] = copy_value [[OPENED]]
// CHECK:   alloc_stack $Optional<@callee_guaranteed @substituted <τ_0_0 where τ_0_0 : AnyObject> () -> @owned τ_0_0 for <[[OPENED_TY]]>>
// CHECK:   dynamic_method_br [[OPENED_COPY]] : $[[OPENED_TY]], #P1.methodReturnsSelf!foreign, bb1, bb2
//
// CHECK: bb{{[0-9]+}}([[ARG:%[0-9]+]] : @owned $@callee_guaranteed @substituted <τ_0_0 where τ_0_0 : AnyObject> () -> @owned τ_0_0 for <[[OPENED_TY]]>):
// CHECK:   [[ERASED:%[0-9]+]] = convert_function [[ARG]] : {{.*}} to $@callee_guaranteed () -> @owned any P1
// CHECK: } // end sil function '$[[THUNK_NAME]]'
func optionalMethodUnboundRefExistential() {
  // Test that we erase to existential bounds.
  var methodRef = P1.methodReturnsSelf
}

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B15PropertyGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalPropertyGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[TLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[TBOX]]
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TLIFETIME]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<Int> }
  // CHECK:   [[OPT_LIFETIME:%.*]] = begin_borrow [var_decl] [[OPT_BOX]]
  // CHECK:   project_box [[OPT_LIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.prop!getter.foreign
  var propertyRef = t.prop
}
// CHECK: } // end sil function '$s17protocol_optional0B15PropertyGeneric{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s17protocol_optional0B16SubscriptGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P1> (@guaranteed T) -> ()
func optionalSubscriptGeneric<T : P1>(t t : T) {
  var t = t
  // CHECK: bb0([[T:%[0-9]+]] : @guaranteed $T):
  // CHECK:   [[TBOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : P1> { var τ_0_0 } <T>
  // CHECK:   [[TLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[TBOX]]
  // CHECK:   [[PT:%[0-9]+]] = project_box [[TLIFETIME]]
  // CHECK:   [[T_COPY:%.*]] = copy_value [[T]]
  // CHECK:   store [[T_COPY]] to [init] [[PT]] : $*T
  // CHECK:   [[OPT_BOX:%[0-9]+]] = alloc_box ${ var Optional<Int> }
  // CHECK:   [[OPT_LIFETIME:%.*]] = begin_borrow [var_decl] [[OPT_BOX]]
  // CHECK:   project_box [[OPT_LIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PT]] : $*T
  // CHECK:   [[T:%[0-9]+]] = load [copy] [[READ]] : $*T
  // CHECK:   [[FIVELIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 5
  // CHECK:   [[INT64:%[0-9]+]] = metatype $@thin Int.Type
  // CHECK:   [[INTCONV:%[0-9]+]] = function_ref @$sSi2{{[_0-9a-zA-Z]*}}fC
  // CHECK:   [[FIVE:%[0-9]+]] = apply [[INTCONV]]([[FIVELIT]], [[INT64]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK:   alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[T]] : $T, #P1.subscript!getter.foreign
  var subscriptRef = t[5]
}
// CHECK: } // end sil function '$s17protocol_optional0B16SubscriptGeneric{{[_0-9a-zA-Z]*}}F'
