// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name dynamic_lookup -enable-objc-interop -parse-as-library -disable-objc-attr-requires-foundation-module -disable-availability-checking %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name dynamic_lookup -enable-objc-interop -parse-as-library -disable-objc-attr-requires-foundation-module -disable-availability-checking  %s | %FileCheck %s --check-prefix=GUARANTEED

// REQUIRES: objc_interop
// REQUIRES: concurrency

class X {
  @objc func f() { }
  @objc class func staticF() { }
  @objc var value: Int {
    return 17
  }

  @objc subscript (i: Int) -> Int {
    get {
      return i
    }
    set {}
  }

  @objc func hasDefaultParam(_ x: Int = 0) {}
}

@objc protocol P {
  func g()
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup15direct_to_class{{[_0-9a-zA-Z]*}}F
func direct_to_class(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  // CHECK: [[OPENED_ARG:%[0-9]+]] = open_existential_ref [[ARG]] : $AnyObject to $@opened({{.*}}, AnyObject) Self
  // CHECK: [[OPENED_ARG_COPY:%.*]] = copy_value [[OPENED_ARG]]
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[OPENED_ARG_COPY]] : $@opened({{.*}}, AnyObject) Self, #X.f!foreign : (X) -> () -> (), $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()
  // CHECK: apply [[METHOD]]([[OPENED_ARG_COPY]]) : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()
  // CHECK: destroy_value [[OPENED_ARG_COPY]]
  obj.f!()
}
// CHECK: } // end sil function '$s14dynamic_lookup15direct_to_class{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup18direct_to_protocol{{[_0-9a-zA-Z]*}}F
func direct_to_protocol(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  // CHECK:   [[OPENED_ARG:%[0-9]+]] = open_existential_ref [[ARG]] : $AnyObject to $@opened({{.*}}, AnyObject) Self
  // CHECK:   [[OPENED_ARG_COPY:%.*]] = copy_value [[OPENED_ARG]]
  // CHECK:   [[METHOD:%[0-9]+]] = objc_method [[OPENED_ARG_COPY]] : $@opened({{.*}}, AnyObject) Self, #P.g!foreign : <Self where Self : P> (Self) -> () -> (), $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()
  // CHECK:   apply [[METHOD]]([[OPENED_ARG_COPY]]) : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()
  // CHECK:   destroy_value [[OPENED_ARG_COPY]]
  obj.g!()
}
// CHECK: } // end sil function '$s14dynamic_lookup18direct_to_protocol{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup23direct_to_static_method{{[_0-9a-zA-Z]*}}F
func direct_to_static_method(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  var obj = obj
  // CHECK: [[OBJBOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK: [[OBJLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJBOX]]
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJLIFETIME]]
  // CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK: store [[ARG_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK: end_access [[READ]]
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick any AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick any AnyObject.Type to $@thick (@opened([[UUID:".*"]], AnyObject) Self).Type
  // CHECK-NEXT: [[METHOD:%[0-9]+]] = objc_method [[OPENMETA]] : $@thick (@opened([[UUID]], AnyObject) Self).Type, #X.staticF!foreign : (X.Type) -> () -> (), $@convention(objc_method) (@thick (@opened([[UUID]], AnyObject) Self).Type) -> ()
  // CHECK: apply [[METHOD]]([[OPENMETA]]) : $@convention(objc_method) (@thick (@opened([[UUID]], AnyObject) Self).Type) -> ()
  // CHECK: end_borrow [[OBJLIFETIME]]
  // CHECK: destroy_value [[OBJBOX]]
  type(of: obj).staticF!()
}
// } // end sil function '_TF14dynamic_lookup23direct_to_static_method{{.*}}'

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup12opt_to_class{{[_0-9a-zA-Z]*}}F
func opt_to_class(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  var obj = obj
  // CHECK:   [[EXISTBOX:%[0-9]+]] = alloc_box ${ var AnyObject } 
  // CHECK:   [[EXISTLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[EXISTBOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[EXISTLIFETIME]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   store [[ARG_COPY]] to [init] [[PBOBJ]]
  // CHECK:   [[OPTBOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   [[OPTLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OPTBOX]]
  // CHECK:   [[PBOPT:%.*]] = project_box [[OPTLIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[EXISTVAL:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_SELF:%[0-9]*]] = open_existential_ref [[EXISTVAL]]
  // CHECK:   [[OPT_TMP:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
  // CHECK:   dynamic_method_br [[OBJ_SELF]] : $@opened({{.*}}, AnyObject) Self, #X.f!foreign, [[HASBB:[a-zA-z0-9]+]], [[NOBB:[a-zA-z0-9]+]]

  // Has method BB:
  // CHECK: [[HASBB]]([[UNCURRIED:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()):
  // CHECK:   [[OBJ_SELF_COPY:%.*]] = copy_value [[OBJ_SELF]]
  // CHECK:   [[PARTIAL:%[0-9]+]] = partial_apply [callee_guaranteed] [[UNCURRIED]]([[OBJ_SELF_COPY]]) : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> ()
  // CHECK:   [[THUNK_PAYLOAD:%.*]] = init_enum_data_addr [[OPT_TMP]]
  // CHECK:   store [[PARTIAL]] to [init] [[THUNK_PAYLOAD]]
  // CHECK:   inject_enum_addr [[OPT_TMP]] : $*Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt
  // CHECK:   br [[CONTBB:[a-zA-Z0-9]+]]

  // No method BB:
  // CHECK: [[NOBB]]:
  // CHECK:   inject_enum_addr [[OPT_TMP]] : {{.*}}, #Optional.none!enumelt
  // CHECK:   br [[CONTBB]]

  // Continuation block
  // CHECK: [[CONTBB]]:
  // CHECK:   [[OPT:%.*]] = load [take] [[OPT_TMP]]
  // CHECK:   store [[OPT]] to [init] [[PBOPT]] : $*Optional<@callee_guaranteed () -> ()>
  // CHECK:   dealloc_stack [[OPT_TMP]]
  var of: (() -> ())! = obj.f

  // Exit
  // CHECK:   destroy_value [[OBJ_SELF]] : $@opened({{".*"}}, AnyObject) Self
  // CHECK:   end_borrow [[OPTLIFETIME]]
  // CHECK:   destroy_value [[OPTBOX]] : ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   end_borrow [[EXISTLIFETIME]]
  // CHECK:   destroy_value [[EXISTBOX]] : ${ var AnyObject }
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup20opt_to_class_unboundyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   metatype $@thin (any AnyObject).Type
// CHECK:   function_ref @$[[THUNK_NAME:[_a-zA-Z0-9]+]]
// CHECK: } // end sil function '$s14dynamic_lookup20opt_to_class_unboundyyF'
//
// CHECK: sil private [ossa] @$[[THUNK_NAME]] : $@convention(thin) (@guaranteed AnyObject) -> @owned Optional<@callee_guaranteed () -> ()> {
// CHECK: bb0(%0 : @guaranteed $AnyObject):
// CHECK:   [[OPENED:%[0-9]+]] = open_existential_ref %0 : $AnyObject to $[[OPENED_TY:@opened\("[-A-F0-9]+", AnyObject\) Self]]
// CHECK:   [[OPENED_COPY:%[0-9]+]] = copy_value [[OPENED]]
// CHECK:   alloc_stack $Optional<@callee_guaranteed () -> ()>
// CHECK:   dynamic_method_br [[OPENED_COPY]] : $[[OPENED_TY]], #X.f!foreign, bb1, bb2
// CHECK: } // end sil function '$[[THUNK_NAME]]'
func opt_to_class_unbound() {
  let f = AnyObject.f
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup20forced_without_outer{{[_0-9a-zA-Z]*}}F
func forced_without_outer(_ obj: AnyObject) {
  // CHECK: dynamic_method_br
  var f = obj.f!
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup20opt_to_static_method{{[_0-9a-zA-Z]*}}F
func opt_to_static_method(_ obj: AnyObject) {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJBOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[OBJLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJBOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJLIFETIME]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[OPTBOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   [[OPTLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OPTBOX]]
  // CHECK:   [[PBO:%.*]] = project_box [[OPTLIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJCOPY:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJMETA:%[0-9]+]] = existential_metatype $@thick any AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK:   [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick any AnyObject.Type to $@thick (@opened
  // CHECK:   [[OBJCMETA:%[0-9]+]] = thick_to_objc_metatype [[OPENMETA]]
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
  // CHECK:   dynamic_method_br [[OBJCMETA]] : $@objc_metatype (@opened({{".*"}}, AnyObject) Self).Type, #X.staticF!foreign, [[HASMETHOD:[A-Za-z0-9_]+]], [[NOMETHOD:[A-Za-z0-9_]+]]
  var optF: (() -> ())! = type(of: obj).staticF
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup15opt_to_property{{[_0-9a-zA-Z]*}}F
func opt_to_property(_ obj: AnyObject) {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[INT_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[INT_LIFETIME:%.*]] = begin_borrow [var_decl] [[INT_BOX]]
  // CHECK:   project_box [[INT_LIFETIME]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}, AnyObject) Self, #X.value!getter.foreign, bb1, bb2

  // CHECK: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> Int):
  // CHECK:   [[RAWOBJ_SELF_COPY:%.*]] = copy_value [[RAWOBJ_SELF]]
  // CHECK:   [[BOUND_METHOD:%[0-9]+]] = partial_apply [callee_guaranteed] [[METHOD]]([[RAWOBJ_SELF_COPY]]) : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> Int
  // CHECK:   [[B:%.*]] = begin_borrow [[BOUND_METHOD]]
  // CHECK:   [[VALUE:%[0-9]+]] = apply [[B]]() : $@callee_guaranteed () -> Int
  // CHECK:   end_borrow [[B]]
  // CHECK:   [[VALUETEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK:   store [[VALUE]] to [trivial] [[VALUETEMP]]
  // CHECK:   inject_enum_addr [[OPTTEMP]]{{.*}}some
  // CHECK:   destroy_value [[BOUND_METHOD]]
  // CHECK:   br bb3
  var i: Int = obj.value!
}
// CHECK: } // end sil function '$s14dynamic_lookup15opt_to_property{{[_0-9a-zA-Z]*}}F'

// GUARANTEED-LABEL: sil hidden [ossa] @$s14dynamic_lookup15opt_to_property{{[_0-9a-zA-Z]*}}F
  // GUARANTEED: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // GUARANTEED:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // GUARANTEED:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // GUARANTEED:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // GUARANTEED:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // GUARANTEED:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // GUARANTEED:   [[INT_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // GUARANTEED:   [[INT_LIFETIME:%.*]] = begin_borrow [var_decl] [[INT_BOX]]
  // GUARANTEED:   project_box [[INT_LIFETIME]]
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // GUARANTEED:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // GUARANTEED:   [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // GUARANTEED:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // GUARANTEED:   dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}, AnyObject) Self, #X.value!getter.foreign, bb1, bb2

  // GUARANTEED: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}, AnyObject) Self) -> Int):
  // GUARANTEED:   [[RAWOBJ_SELF_COPY:%.*]] = copy_value [[RAWOBJ_SELF]]
  // GUARANTEED:   [[BOUND_METHOD:%[0-9]+]] = partial_apply [callee_guaranteed] [[METHOD]]([[RAWOBJ_SELF_COPY]])
  // GUARANTEED:   [[BEGIN_BORROW:%.*]] = begin_borrow [[BOUND_METHOD]]
  // GUARANTEED:   [[VALUE:%[0-9]+]] = apply [[BEGIN_BORROW]]
  // GUARANTEED:   end_borrow [[BEGIN_BORROW]]
  // GUARANTEED:   [[VALUETEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // GUARANTEED:   store [[VALUE]] to [trivial] [[VALUETEMP]]
  // GUARANTEED:   inject_enum_addr [[OPTTEMP]]{{.*}}some
  // GUARANTEED:   destroy_value [[BOUND_METHOD]]
  // GUARANTEED:   br bb3

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup19direct_to_subscript{{[_0-9a-zA-Z]*}}F
func direct_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[I_LIFETIME:%.*]] = begin_borrow [var_decl] [[I_BOX]]
  // CHECK:   [[PBI:%.*]] = project_box [[I_LIFETIME]]
  // CHECK:   store [[I]] to [trivial] [[PBI]] : $*Int
  // CHECK:   alloc_box ${ var Int }
  // CHECK:   project_box
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}, AnyObject) Self
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // CHECK:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}, AnyObject) Self, #X.subscript!getter.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}, AnyObject) Self) -> Int):
  // CHECK:   [[OBJ_REF_COPY:%.*]] = copy_value [[OBJ_REF]]
  // CHECK:   [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [callee_guaranteed] [[GETTER]]([[OBJ_REF_COPY]]) : $@convention(objc_method) (Int, @opened({{.*}}, AnyObject) Self) -> Int
  // CHECK:   [[B:%.*]] = begin_borrow [[GETTER_WITH_SELF]]
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[B]]([[I]]) : $@callee_guaranteed (Int) -> Int
  // CHECK:   end_borrow [[B]]
  // CHECK:   [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK:   store [[RESULT]] to [trivial] [[RESULTTEMP]]
  // CHECK:   inject_enum_addr [[OPTTEMP]]{{.*}}some
  // CHECK:   destroy_value [[GETTER_WITH_SELF]]
  // CHECK:   br bb3
  var x: Int = obj[i]!
}
// CHECK: } // end sil function '$s14dynamic_lookup19direct_to_subscript{{[_0-9a-zA-Z]*}}F'

// GUARANTEED-LABEL: sil hidden [ossa] @$s14dynamic_lookup19direct_to_subscript{{[_0-9a-zA-Z]*}}F
  // GUARANTEED: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // GUARANTEED:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // GUARANTEED:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // GUARANTEED:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // GUARANTEED:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // GUARANTEED:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // GUARANTEED:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // GUARANTEED:   [[I_LIFETIME:%.*]] = begin_borrow [var_decl] [[I_BOX]]
  // GUARANTEED:   [[PBI:%.*]] = project_box [[I_LIFETIME]]
  // GUARANTEED:   store [[I]] to [trivial] [[PBI]] : $*Int
  // GUARANTEED:   alloc_box ${ var Int }
  // GUARANTEED:   project_box
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // GUARANTEED:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // GUARANTEED:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}, AnyObject) Self
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // GUARANTEED:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // GUARANTEED:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // GUARANTEED:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}, AnyObject) Self, #X.subscript!getter.foreign, bb1, bb2

  // GUARANTEED: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}, AnyObject) Self) -> Int):
  // GUARANTEED:   [[OBJ_REF_COPY:%.*]] = copy_value [[OBJ_REF]]
  // GUARANTEED:   [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [callee_guaranteed] [[GETTER]]([[OBJ_REF_COPY]])
  // GUARANTEED:   [[BORROW:%.*]] = begin_borrow [[GETTER_WITH_SELF]]
  // GUARANTEED:   [[RESULT:%[0-9]+]] = apply [[BORROW]]([[I]])
  // GUARANTEED:   end_borrow [[BORROW]]
  // GUARANTEED:   [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // GUARANTEED:   store [[RESULT]] to [trivial] [[RESULTTEMP]]
  // GUARANTEED:   inject_enum_addr [[OPTTEMP]]{{.*}}some
  // GUARANTEED:   destroy_value [[GETTER_WITH_SELF]]
  // GUARANTEED:   br bb3

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup16opt_to_subscript{{[_0-9a-zA-Z]*}}F
func opt_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[I_LIFETIME:%.*]] = begin_borrow [var_decl] [[I_BOX]]
  // CHECK:   [[PBI:%.*]] = project_box [[I_LIFETIME]]
  // CHECK:   store [[I]] to [trivial] [[PBI]] : $*Int
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}, AnyObject) Self
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // CHECK:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}, AnyObject) Self, #X.subscript!getter.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}, AnyObject) Self) -> Int):
  // CHECK:   [[OBJ_REF_COPY:%.*]] = copy_value [[OBJ_REF]]
  // CHECK:   [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [callee_guaranteed] [[GETTER]]([[OBJ_REF_COPY]]) : $@convention(objc_method) (Int, @opened({{.*}}, AnyObject) Self) -> Int
  // CHECK:   [[B:%.*]] = begin_borrow [[GETTER_WITH_SELF]]
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[B]]([[I]]) : $@callee_guaranteed (Int) -> Int
  // CHECK:   end_borrow [[B]]
  // CHECK:   [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK:   store [[RESULT]] to [trivial] [[RESULTTEMP]]
  // CHECK:   inject_enum_addr [[OPTTEMP]]
  // CHECK:   destroy_value [[GETTER_WITH_SELF]]
  // CHECK:   br bb3
  obj[i]
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup8downcast{{[_0-9a-zA-Z]*}}F
func downcast(_ obj: AnyObject) -> X {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[OBJ_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[OBJ_BOX]]
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_LIFETIME]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[X:%[0-9]+]] = unconditional_checked_cast [[OBJ]] : $AnyObject to X
  // CHECK:   end_borrow [[OBJ_LIFETIME]]
  // CHECK:   destroy_value [[OBJ_BOX]] : ${ var AnyObject }
  // CHECK:   return [[X]] : $X
  return obj as! X
}

@objc class Juice { }

@objc protocol Fruit {
  @objc optional var juice: Juice { get }
}

// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup7consumeyyAA5Fruit_pF
// CHECK: bb0(%0 : @guaranteed $any Fruit):
// CHECK:        [[BOX:%.*]] = alloc_stack $Optional<Juice>
// CHECK:        dynamic_method_br [[SELF:%.*]] : $@opened("{{.*}}", any Fruit) Self, #Fruit.juice!getter.foreign, bb1, bb2

// CHECK: bb1([[FN:%.*]] : $@convention(objc_method) (@opened("{{.*}}", any Fruit) Self) -> @autoreleased Juice):
// CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:   [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]]([[SELF_COPY]]) : $@convention(objc_method) (@opened("{{.*}}", any Fruit) Self) -> @autoreleased Juice
// CHECK:   [[B:%.*]] = begin_borrow [[METHOD]]
// CHECK:   [[RESULT:%.*]] = apply [[B]]() : $@callee_guaranteed () -> @owned Juice
// CHECK:   end_borrow [[B]]
// CHECK:   [[PAYLOAD:%.*]] = init_enum_data_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt
// CHECK:   store [[RESULT]] to [init] [[PAYLOAD]]
// CHECK:   inject_enum_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt
// CHECK:   destroy_value [[METHOD]]
// CHECK:   br bb3

// CHECK: bb2:
// CHECK:        inject_enum_addr [[BOX]] : $*Optional<Juice>, #Optional.none!enumelt
// CHECK:        br bb3

// CHECK: bb3:
// CHECK:        return

func consume(_ fruit: Fruit) {
  _ = fruit.juice
}

// rdar://problem/29249513 -- looking up an IUO member through AnyObject
// produces a Foo!? type. The SIL verifier did not correctly consider Optional
// to be the lowering of IUO (which is now eliminated by SIL lowering).

@objc protocol IUORequirement {
  var iuoProperty: AnyObject! { get }
}

func getIUOPropertyDynamically(x: AnyObject) -> Any {
  return x.iuoProperty
}

// https://github.com/apple/swift/issues/54059
//
// CHECK-LABEL: sil hidden [ossa] @$s14dynamic_lookup24testAnyObjectWithDefaultyyyXlF
func testAnyObjectWithDefault(_ x: AnyObject) {
  // CHECK: function_ref default argument 0 of X.hasDefaultParam(_:)
  // CHECK: [[DEFGEN:%[0-9]+]] = function_ref @$s14dynamic_lookup1XC15hasDefaultParamyySiFfA_ : $@convention(thin) () -> Int
  // CHECK: [[DEFARG:%[0-9]+]] = apply %4() : $@convention(thin) () -> Int
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[OPENEDX:%[0-9]+]] : $@opened("{{.*}}", AnyObject) Self, #X.hasDefaultParam!foreign : (X) -> (Int) -> (), $@convention(objc_method) (Int, @opened("{{.*}}", AnyObject) Self) -> ()
  // CHECK: apply [[METHOD]]([[DEFARG]], [[OPENEDX]])
  x.hasDefaultParam()
}
