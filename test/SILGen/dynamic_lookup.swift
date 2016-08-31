// RUN: %target-swift-frontend -parse-as-library -emit-silgen -disable-objc-attr-requires-foundation-module %s | %FileCheck %s

// REQUIRES: objc_interop

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

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup15direct_to_class
func direct_to_class(_ obj: AnyObject) {
  // CHECK: [[OBJ_SELF:%[0-9]+]] = open_existential_ref [[EX:%[0-9]+]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign : (X) -> () -> (), $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK: apply [[METHOD]]([[OBJ_SELF]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  obj.f!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup18direct_to_protocol
func direct_to_protocol(_ obj: AnyObject) {
  // CHECK: [[OBJ_SELF:%[0-9]+]] = open_existential_ref [[EX:%[0-9]+]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #P.g!1.foreign : <Self where Self : P> (Self) -> () -> (), $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK: apply [[METHOD]]([[OBJ_SELF]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  obj.g!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup23direct_to_static_method
func direct_to_static_method(_ obj: AnyObject) {
  var obj = obj
  // CHECK: [[START:[A-Za-z0-9_]+]]([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK: [[OBJBOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJBOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick (@opened([[UUID:".*"]]) AnyObject).Type
  // CHECK-NEXT: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OPENMETA]] : $@thick (@opened([[UUID]]) AnyObject).Type, #X.staticF!1.foreign : (X.Type) -> () -> (), $@convention(objc_method) (@thick (@opened([[UUID]]) AnyObject).Type) -> ()
  // CHECK: apply [[METHOD]]([[OPENMETA]]) : $@convention(objc_method) (@thick (@opened([[UUID]]) AnyObject).Type) -> ()
  type(of: obj).staticF!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup12opt_to_class
func opt_to_class(_ obj: AnyObject) {
  var obj = obj
  // CHECK: [[ENTRY:[A-Za-z0-9]+]]([[PARAM:%[0-9]+]] : $AnyObject)
  // CHECK: [[EXISTBOX:%[0-9]+]] = alloc_box $AnyObject 
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[EXISTBOX]]
  // CHECK: store [[PARAM]] to [[PBOBJ]]
  // CHECK-NEXT: [[OPTBOX:%[0-9]+]] = alloc_box $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: [[PBOPT:%.*]] = project_box [[OPTBOX]]
  // CHECK-NEXT: [[EXISTVAL:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: strong_retain [[EXISTVAL]] : $AnyObject
  // CHECK-NEXT: [[OBJ_SELF:%[0-9]*]] = open_existential_ref [[EXIST:%[0-9]+]]
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dynamic_method_br [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign, [[HASBB:[a-zA-z0-9]+]], [[NOBB:[a-zA-z0-9]+]]

  // Has method BB:
  // CHECK: [[HASBB]]([[UNCURRIED:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()):
  // CHECK-NEXT: strong_retain [[OBJ_SELF]]
  // CHECK-NEXT: [[PARTIAL:%[0-9]+]] = partial_apply [[UNCURRIED]]([[OBJ_SELF]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> ()
  // CHECK-NEXT: [[THUNK_PAYLOAD:%.*]] = init_enum_data_addr [[OPTIONAL:%[0-9]+]]
  // CHECK:      [[THUNKFN:%.*]] = function_ref @{{.*}} : $@convention(thin) (@in (), @owned @callee_owned () -> ()) -> @out ()
  // CHECK-NEXT: [[THUNK:%.*]] = partial_apply [[THUNKFN]]([[PARTIAL]])
  // CHECK-NEXT: store [[THUNK]] to [[THUNK_PAYLOAD]]
  // CHECK-NEXT: inject_enum_addr [[OPTIONAL]]{{.*}}some
  // CHECK-NEXT: br [[CONTBB:[a-zA-Z0-9]+]]
  
  // No method BB:
  // CHECK: [[NOBB]]:
  // CHECK-NEXT: inject_enum_addr [[OPTIONAL]]{{.*}}none
  // CHECK-NEXT: br [[CONTBB]]

  // Continuation block
  // CHECK: [[CONTBB]]:
  // CHECK-NEXT: [[OPT:%.*]] = load [[OPTTEMP]]
  // CHECK-NEXT: store [[OPT]] to [[PBOPT]] : $*ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dealloc_stack [[OPTTEMP]]
  var of: (() -> ())! = obj.f

  // Exit
  // CHECK-NEXT: strong_release [[OBJ_SELF]] : $@opened({{".*"}}) AnyObject
  // CHECK-NEXT: strong_release [[OPTBOX]] : $@box ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: strong_release [[EXISTBOX]] : $@box AnyObject
  // CHECK-NEXT: strong_release %0
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[RESULT]] : $()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup20forced_without_outer
func forced_without_outer(_ obj: AnyObject) {
  // CHECK: dynamic_method_br
  var f = obj.f!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup20opt_to_static_method
func opt_to_static_method(_ obj: AnyObject) {
  var obj = obj
  // CHECK: [[ENTRY:[A-Za-z0-9]+]]([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK: [[OBJBOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJBOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[OPTBOX:%[0-9]+]] = alloc_box $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: [[PBO:%.*]] = project_box [[OPTBOX]]
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick (@opened
  // CHECK-NEXT: [[OBJCMETA:%[0-9]+]] = thick_to_objc_metatype [[OPENMETA]]
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dynamic_method_br [[OBJCMETA]] : $@objc_metatype (@opened({{".*"}}) AnyObject).Type, #X.staticF!1.foreign, [[HASMETHOD:[A-Za-z0-9_]+]], [[NOMETHOD:[A-Za-z0-9_]+]]
  var optF: (() -> ())! = type(of: obj).staticF
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup15opt_to_property
func opt_to_property(_ obj: AnyObject) {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[INT_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: project_box [[INT_BOX]]
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.value!getter.1.foreign, bb1, bb2
  // CHECK: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[RAWOBJ_SELF]]
  // CHECK-NEXT: [[BOUND_METHOD:%[0-9]+]] = partial_apply [[METHOD]]([[RAWOBJ_SELF]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[VALUE:%[0-9]+]] = apply [[BOUND_METHOD]]() : $@callee_owned () -> Int
  // CHECK-NEXT: [[VALUETEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[VALUE]] to [[VALUETEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]{{.*}}some
  // CHECK-NEXT: br bb3
  var i: Int = obj.value!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup19direct_to_subscript
func direct_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[I_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: [[PBI:%.*]] = project_box [[I_BOX]]
  // CHECK-NEXT: store [[I]] to [[PBI]] : $*Int
  // CHECK-NEXT: alloc_box $Int
  // CHECK-NEXT: project_box
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK-NEXT: [[I:%[0-9]+]] = load [[PBI]] : $*Int
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[OBJ_REF]]
  // CHECK-NEXT: [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [[GETTER]]([[OBJ_REF]]) : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[GETTER_WITH_SELF]]([[I]]) : $@callee_owned (Int) -> Int
  // CHECK-NEXT: [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[RESULT]] to [[RESULTTEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]{{.*}}some
  // CHECK-NEXT: br bb3
  var x: Int = obj[i]!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup16opt_to_subscript
func opt_to_subscript(_ obj: AnyObject, i: Int) {
  var obj = obj
  var i = i
  // CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[I_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: [[PBI:%.*]] = project_box [[I_BOX]]
  // CHECK-NEXT: store [[I]] to [[PBI]] : $*Int
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK-NEXT: [[I:%[0-9]+]] = load [[PBI]] : $*Int
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[OBJ_REF]]
  // CHECK-NEXT: [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [[GETTER]]([[OBJ_REF]]) : $@convention(objc_method) (Int, @opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[GETTER_WITH_SELF]]([[I]]) : $@callee_owned (Int) -> Int
  // CHECK-NEXT: [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[RESULT]] to [[RESULTTEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]
  // CHECK-NEXT: br bb3
  obj[i]
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup8downcast
func downcast(_ obj: AnyObject) -> X {
  var obj = obj
  // CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: [[PBOBJ:%[0-9]+]] = project_box [[OBJ_BOX]]
  // CHECK: store [[OBJ]] to [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[PBOBJ]] : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[X:%[0-9]+]] = unconditional_checked_cast [[OBJ]] : $AnyObject to $X
  // CHECK-NEXT: strong_release [[OBJ_BOX]] : $@box AnyObject
  // CHECK-NEXT: strong_release %0
  // CHECK-NEXT: return [[X]] : $X
  return obj as! X
}

@objc class Juice { }

@objc protocol Fruit {
  @objc optional var juice: Juice { get }
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup7consumeFPS_5Fruit_T_
// CHECK: bb0(%0 : $Fruit):
// CHECK:        [[BOX:%.*]] = alloc_stack $Optional<Juice>
// CHECK:        dynamic_method_br [[SELF:%.*]] : $@opened("{{.*}}") Fruit, #Fruit.juice!getter.1.foreign, bb1, bb2

// CHECK: bb1([[FN:%.*]] : $@convention(objc_method) (@opened("{{.*}}") Fruit) -> @autoreleased Juice):
// CHECK:        [[METHOD:%.*]] = partial_apply [[FN]]([[SELF]]) : $@convention(objc_method) (@opened("{{.*}}") Fruit) -> @autoreleased Juice
// CHECK:        [[RESULT:%.*]] = apply [[METHOD]]() : $@callee_owned () -> @owned Juice
// CHECK:        [[PAYLOAD:%.*]] = init_enum_data_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt.1
// CHECK:        store [[RESULT]] to [[PAYLOAD]]
// CHECK:        inject_enum_addr [[BOX]] : $*Optional<Juice>, #Optional.some!enumelt.1
// CHECK:        br bb3

// CHECK: bb2:
// CHECK:        inject_enum_addr [[BOX]] : $*Optional<Juice>, #Optional.none!enumelt
// CHECK:        br bb3

// CHECK: bb3:
// CHECK:        return

func consume(_ fruit: Fruit) {
  _ = fruit.juice
}
