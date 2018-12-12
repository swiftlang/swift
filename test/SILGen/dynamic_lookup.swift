// RUN: %target-swift-emit-silgen -module-name dynamic_lookup -enable-objc-interop -parse-as-library -disable-objc-attr-requires-foundation-module %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -module-name dynamic_lookup -enable-objc-interop -parse-as-library -disable-objc-attr-requires-foundation-module  %s | %FileCheck %s --check-prefix=GUARANTEED

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
}

@objc protocol P {
  func g()
}

// CHECK-LABEL: sil hidden @$s14dynamic_lookup15direct_to_class{{[_0-9a-zA-Z]*}}F
func direct_to_class(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  // CHECK: [[OPENED_ARG:%[0-9]+]] = open_existential_ref [[ARG]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK: [[OPENED_ARG_COPY:%.*]] = copy_value [[OPENED_ARG]]
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[OPENED_ARG_COPY]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign : (X) -> () -> (), $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK: apply [[METHOD]]([[OPENED_ARG_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK: destroy_value [[OPENED_ARG_COPY]]
  obj.f!()
}
// CHECK: } // end sil function '$s14dynamic_lookup15direct_to_class{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden @$s14dynamic_lookup18direct_to_protocol{{[_0-9a-zA-Z]*}}F
func direct_to_protocol(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  // CHECK:   [[OPENED_ARG:%[0-9]+]] = open_existential_ref [[ARG]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK:   [[OPENED_ARG_COPY:%.*]] = copy_value [[OPENED_ARG]]
  // CHECK:   [[METHOD:%[0-9]+]] = objc_method [[OPENED_ARG_COPY]] : $@opened({{.*}}) AnyObject, #P.g!1.foreign : <Self where Self : P> (Self) -> () -> (), $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK:   apply [[METHOD]]([[OPENED_ARG_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK:   destroy_value [[OPENED_ARG_COPY]]
  obj.g!()
}
// CHECK: } // end sil function '$s14dynamic_lookup18direct_to_protocol{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden @$s14dynamic_lookup23direct_to_static_method{{[_0-9a-zA-Z]*}}F
func direct_to_static_method(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  var obj = obj
  // CHECK: [[OBJBOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJBOX]]
  // CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK: store [[ARG_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK: end_access [[READ]]
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick (@opened([[UUID:".*"]]) AnyObject).Type
  // CHECK-NEXT: [[METHOD:%[0-9]+]] = objc_method [[OPENMETA]] : $@thick (@opened([[UUID]]) AnyObject).Type, #X.staticF!1.foreign : (X.Type) -> () -> (), $@convention(objc_method) (@thick (@opened([[UUID]]) AnyObject).Type) -> ()
  // CHECK: apply [[METHOD]]([[OPENMETA]]) : $@convention(objc_method) (@thick (@opened([[UUID]]) AnyObject).Type) -> ()
  // CHECK: destroy_value [[OBJBOX]]
  type(of: obj).staticF!()
}
// } // end sil function '_TF14dynamic_lookup23direct_to_static_method{{.*}}'

// CHECK-LABEL: sil hidden @$s14dynamic_lookup12opt_to_class{{[_0-9a-zA-Z]*}}F
func opt_to_class(_ obj: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject):
  var obj = obj
  // CHECK:   [[EXISTBOX:%[0-9]+]] = alloc_box ${ var AnyObject } 
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[EXISTBOX]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   store [[ARG_COPY]] to [init] [[PBOBJ]]
  // CHECK:   [[OPTBOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   [[PBOPT:%.*]] = project_box [[OPTBOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[EXISTVAL:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_SELF:%[0-9]*]] = open_existential_ref [[EXISTVAL]]
  // CHECK:   [[OPT_TMP:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
  // CHECK:   dynamic_method_br [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign, [[HASBB:[a-zA-z0-9]+]], [[NOBB:[a-zA-z0-9]+]]

  // Has method BB:
  // CHECK: [[HASBB]]([[UNCURRIED:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()):
  // CHECK:   [[OBJ_SELF_COPY:%.*]] = copy_value [[OBJ_SELF]]
  // CHECK:   [[PARTIAL:%[0-9]+]] = partial_apply [callee_guaranteed] [[UNCURRIED]]([[OBJ_SELF_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK:   [[THUNK_PAYLOAD:%.*]] = init_enum_data_addr [[OPT_TMP]]
  // CHECK:   store [[PARTIAL]] to [init] [[THUNK_PAYLOAD]]
  // CHECK:   inject_enum_addr [[OPT_TMP]] : $*Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt.1
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
  // CHECK:   destroy_value [[OBJ_SELF]] : $@opened({{".*"}}) AnyObject
  // CHECK:   destroy_value [[OPTBOX]] : ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   destroy_value [[EXISTBOX]] : ${ var AnyObject }
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
}

// CHECK-LABEL: sil hidden @$s14dynamic_lookup20forced_without_outer{{[_0-9a-zA-Z]*}}F
func forced_without_outer(_ obj: AnyObject) {
  // CHECK: dynamic_method_br
  var f = obj.f!
}

// CHECK-LABEL: sil hidden @$s14dynamic_lookup20opt_to_static_method{{[_0-9a-zA-Z]*}}F
func opt_to_static_method(_ obj: AnyObject) {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJBOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJBOX]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[OPTBOX:%[0-9]+]] = alloc_box ${ var Optional<@callee_guaranteed () -> ()> }
  // CHECK:   [[PBO:%.*]] = project_box [[OPTBOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJCOPY:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK:   [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick (@opened
  // CHECK:   [[OBJCMETA:%[0-9]+]] = thick_to_objc_metatype [[OPENMETA]]
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
  // CHECK:   dynamic_method_br [[OBJCMETA]] : $@objc_metatype (@opened({{".*"}}) AnyObject).Type, #X.staticF!1.foreign, [[HASMETHOD:[A-Za-z0-9_]+]], [[NOMETHOD:[A-Za-z0-9_]+]]
  var optF: (() -> ())! = type(of: obj).staticF
}

// CHECK-LABEL: sil hidden @$s14dynamic_lookup15opt_to_property{{[_0-9a-zA-Z]*}}F
func opt_to_property(_ obj: AnyObject) {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[INT_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   project_box [[INT_BOX]]
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.value!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> Int):
  // CHECK:   [[RAWOBJ_SELF_COPY:%.*]] = copy_value [[RAWOBJ_SELF]]
  // CHECK:   [[BOUND_METHOD:%[0-9]+]] = partial_apply [callee_guaranteed] [[METHOD]]([[RAWOBJ_SELF_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> Int
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

// GUARANTEED-LABEL: sil hidden @$s14dynamic_lookup15opt_to_property{{[_0-9a-zA-Z]*}}F
  // GUARANTEED: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // GUARANTEED:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // GUARANTEED:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // GUARANTEED:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // GUARANTEED:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // GUARANTEED:   [[INT_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // GUARANTEED:   project_box [[INT_BOX]]
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // GUARANTEED:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // GUARANTEED:   [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // GUARANTEED:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // GUARANTEED:   dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.value!getter.1.foreign, bb1, bb2

  // GUARANTEED: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> Int):
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

// CHECK-LABEL: sil hidden @$s14dynamic_lookup19direct_to_subscript{{[_0-9a-zA-Z]*}}F
func direct_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[PBI:%.*]] = project_box [[I_BOX]]
  // CHECK:   store [[I]] to [trivial] [[PBI]] : $*Int
  // CHECK:   alloc_box ${ var Int }
  // CHECK:   project_box
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // CHECK:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK:   [[OBJ_REF_COPY:%.*]] = copy_value [[OBJ_REF]]
  // CHECK:   [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [callee_guaranteed] [[GETTER]]([[OBJ_REF_COPY]]) : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int
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

// GUARANTEED-LABEL: sil hidden @$s14dynamic_lookup19direct_to_subscript{{[_0-9a-zA-Z]*}}F
  // GUARANTEED: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // GUARANTEED:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // GUARANTEED:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // GUARANTEED:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // GUARANTEED:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // GUARANTEED:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // GUARANTEED:   [[PBI:%.*]] = project_box [[I_BOX]]
  // GUARANTEED:   store [[I]] to [trivial] [[PBI]] : $*Int
  // GUARANTEED:   alloc_box ${ var Int }
  // GUARANTEED:   project_box
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // GUARANTEED:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // GUARANTEED:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // GUARANTEED:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // GUARANTEED:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // GUARANTEED:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // GUARANTEED:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // GUARANTEED: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
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

// CHECK-LABEL: sil hidden @$s14dynamic_lookup16opt_to_subscript{{[_0-9a-zA-Z]*}}F
func opt_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[I_BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[PBI:%.*]] = project_box [[I_BOX]]
  // CHECK:   store [[I]] to [trivial] [[PBI]] : $*Int
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBI]]
  // CHECK:   [[I:%[0-9]+]] = load [trivial] [[READ]] : $*Int
  // CHECK:   [[OPTTEMP:%.*]] = alloc_stack $Optional<Int>
  // CHECK:   dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK:   [[OBJ_REF_COPY:%.*]] = copy_value [[OBJ_REF]]
  // CHECK:   [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [callee_guaranteed] [[GETTER]]([[OBJ_REF_COPY]]) : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int
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

// CHECK-LABEL: sil hidden @$s14dynamic_lookup8downcast{{[_0-9a-zA-Z]*}}F
func downcast(_ obj: AnyObject) -> X {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : @guaranteed $AnyObject):
  // CHECK:   [[OBJ_BOX:%[0-9]+]] = alloc_box ${ var AnyObject }
  // CHECK:   [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK:   [[OBJ_COPY:%.*]] = copy_value [[OBJ]]
  // CHECK:   store [[OBJ_COPY]] to [init] [[PBOBJ]] : $*AnyObject
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBOBJ]]
  // CHECK:   [[OBJ:%[0-9]+]] = load [copy] [[READ]] : $*AnyObject
  // CHECK:   [[X:%[0-9]+]] = unconditional_checked_cast [[OBJ]] : $AnyObject to $X
  // CHECK:   destroy_value [[OBJ_BOX]] : ${ var AnyObject }
  // CHECK:   return [[X]] : $X
  return obj as! X
}

@objc class Juice { }

@objc protocol Fruit {
  @objc optional var juice: Juice { get }
}

// CHECK-LABEL: sil hidden @$s14dynamic_lookup7consumeyyAA5Fruit_pF
// CHECK: bb0(%0 : @guaranteed $Fruit):
// CHECK:        [[BOX:%.*]] = alloc_stack $Optional<Juice>
// CHECK:        dynamic_method_br [[SELF:%.*]] : $@opened("{{.*}}") Fruit, #Fruit.juice!getter.1.foreign, bb1, bb2

// CHECK: bb1([[FN:%.*]] : $@convention(objc_method) (@opened("{{.*}}") Fruit) -> @autoreleased Juice):
// CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:   [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]]([[SELF_COPY]]) : $@convention(objc_method) (@opened("{{.*}}") Fruit) -> @autoreleased Juice
// CHECK:   [[B:%.*]] = begin_borrow [[METHOD]]
// CHECK:   [[RESULT:%.*]] = apply [[B]]() : $@callee_guaranteed () -> @owned Juice
// CHECK:   end_borrow [[B]]
// CHECK:   [[PAYLOAD:%.*]] = init_enum_data_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt.1
// CHECK:   store [[RESULT]] to [init] [[PAYLOAD]]
// CHECK:   inject_enum_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt.1
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

