// RUN: %target-swift-frontend -parse-as-library -emit-silgen -disable-objc-attr-requires-foundation-module %s | FileCheck %s

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
func direct_to_class(obj: AnyObject) {
  // CHECK: [[OBJ_SELF:%[0-9]+]] = open_existential_ref [[EX:%[0-9]+]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign : X -> () -> (), $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()
  // CHECK: apply [[METHOD]]([[OBJ_SELF]]) : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()
  obj.f!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup18direct_to_protocol
func direct_to_protocol(obj: AnyObject) {
  // CHECK: [[OBJ_SELF:%[0-9]+]] = open_existential_ref [[EX:%[0-9]+]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #P.g!1.foreign : <`Self` : P> Self -> () -> (), $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()
  // CHECK: apply [[METHOD]]([[OBJ_SELF]]) : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()
  obj.g!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup23direct_to_static_method
func direct_to_static_method(var obj: AnyObject) {
  // CHECK: [[START:[A-Za-z0-9_]+]]([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK-NEXT: [[OBJBOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJBOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [[OBJBOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick @opened([[UUID:".*"]]) AnyObject.Type
  // CHECK-NEXT: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OPENMETA]] : $@thick @opened([[UUID]]) AnyObject.Type, #X.staticF!1.foreign : X.Type -> () -> (), $@cc(objc_method) @thin (@thick @opened([[UUID]]) AnyObject.Type) -> ()
  // CHECK: apply [[METHOD]]([[OPENMETA]]) : $@cc(objc_method) @thin (@thick @opened([[UUID]]) AnyObject.Type) -> ()
  obj.dynamicType.staticF!()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup12opt_to_class
func opt_to_class(var obj: AnyObject) {
  // CHECK: [[ENTRY:[A-Za-z0-9]+]]([[PARAM:%[0-9]+]] : $AnyObject)
  // CHECK-NEXT: [[EXISTBOX:%[0-9]+]] = alloc_box $AnyObject 
  // CHECK-NEXT: store [[PARAM]] to [[EXISTBOX]]#1
  // CHECK-NEXT: [[OPTBOX:%[0-9]+]] = alloc_box $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: [[EXISTVAL:%[0-9]+]] = load [[EXISTBOX]]#1 : $*AnyObject
  // CHECK-NEXT: strong_retain [[EXISTVAL]] : $AnyObject
  // CHECK-NEXT: [[OBJ_SELF:%[0-9]*]] = open_existential_ref [[EXIST:%[0-9]+]]
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dynamic_method_br [[OBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.f!1.foreign, [[HASBB:[a-zA-z0-9]+]], [[NOBB:[a-zA-z0-9]+]]

  // Has method BB:
  // CHECK: [[HASBB]]([[UNCURRIED:%[0-9]+]] : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()):
  // CHECK-NEXT: strong_retain [[OBJ_SELF]]
  // CHECK-NEXT: [[PARTIAL:%[0-9]+]] = partial_apply [[UNCURRIED]]([[OBJ_SELF]]) : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> ()
  // CHECK-NEXT: [[THUNK_PAYLOAD:%.*]] = init_enum_data_addr [[OPTIONAL:%[0-9]+]]
  // CHECK:      [[THUNKFN:%.*]] = function_ref @{{.*}} : $@thin (@out (), @in (), @owned @callee_owned () -> ()) -> ()
  // CHECK-NEXT: [[THUNK:%.*]] = partial_apply [[THUNKFN]]([[PARTIAL]])
  // CHECK-NEXT: store [[THUNK]] to [[THUNK_PAYLOAD]]
  // CHECK-NEXT: inject_enum_addr [[OPTIONAL]]{{.*}}Some
  // CHECK-NEXT: br [[CONTBB:[a-zA-Z0-9]+]]
  
  // No method BB:
  // CHECK: [[NOBB]]:
  // CHECK-NEXT: inject_enum_addr [[OPTIONAL]]{{.*}}None
  // CHECK-NEXT: br [[CONTBB]]

  // Continuation block
  // CHECK: [[CONTBB]]:
  // CHECK-NEXT: [[OPT:%.*]] = load [[OPTTEMP]]#1
  // CHECK-NEXT: store [[OPT]] to [[OPTBOX]]#1 : $*ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dealloc_stack [[OPTTEMP]]#0
  var of = obj.f

  // Exit
  // CHECK-NEXT: strong_release [[OBJ_SELF]] : $@opened({{".*"}}) AnyObject
  // CHECK-NEXT: strong_release [[OPTBOX]]#0 : $Builtin.NativeObject
  // CHECK-NEXT: strong_release [[EXISTBOX]]#0 : $Builtin.NativeObject
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[RESULT]] : $()
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup20forced_without_outer
func forced_without_outer(obj: AnyObject) {
  // CHECK: dynamic_method_br
  var f = obj.f!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup20opt_to_static_method
func opt_to_static_method(var obj: AnyObject) {
  // CHECK: [[ENTRY:[A-Za-z0-9]+]]([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK-NEXT: [[OBJBOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJBOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[OPTBOX:%[0-9]+]] = alloc_box $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: [[OBJCOPY:%[0-9]+]] = load [[OBJBOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[OBJMETA:%[0-9]+]] = existential_metatype $@thick AnyObject.Type, [[OBJCOPY]] : $AnyObject
  // CHECK-NEXT: [[OPENMETA:%[0-9]+]] = open_existential_metatype [[OBJMETA]] : $@thick AnyObject.Type to $@thick @opened
  // CHECK-NEXT: [[OBJCMETA:%[0-9]+]] = thick_to_objc_metatype [[OPENMETA]]
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<() -> ()>
  // CHECK-NEXT: dynamic_method_br [[OBJCMETA]] : $@objc_metatype @opened({{".*"}}) AnyObject.Type, #X.staticF!1.foreign, [[HASMETHOD:[A-Za-z0-9_]+]], [[NOMETHOD:[A-Za-z0-9_]+]]
  var optF = obj.dynamicType.staticF
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup15opt_to_property
func opt_to_property(var obj: AnyObject) {
  // CHECK-NEXT: bb0([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK-NEXT: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[INT_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: [[UNKNOWN_USE:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[RAWOBJ_SELF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[RAWOBJ_SELF]] : $@opened({{.*}}) AnyObject, #X.value!getter.1.foreign, bb1, bb2
  // CHECK: bb1([[METHOD:%[0-9]+]] : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[RAWOBJ_SELF]]
  // CHECK-NEXT: [[BOUND_METHOD:%[0-9]+]] = partial_apply [[METHOD]]([[RAWOBJ_SELF]]) : $@cc(objc_method) @thin (@opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[VALUE:%[0-9]+]] = apply [[BOUND_METHOD]]() : $@callee_owned () -> Int
  // CHECK-NEXT: [[VALUETEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[VALUE]] to [[VALUETEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]{{.*}}Some
  // CHECK-NEXT: br bb3
  var i: Int = obj.value!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup19direct_to_subscript
func direct_to_subscript(var obj: AnyObject, var i: Int) {
  // CHECK-NEXT: bb0([[OBJ:%[0-9]+]] : $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK-NEXT: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[I_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: store [[I]] to [[I_BOX]]#1 : $*Int
  // CHECK-NEXT: alloc_box $Int
  // CHECK-NEXT: [[UNKNOWN_USE:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK-NEXT: [[I:%[0-9]+]] = load [[I_BOX]]#1 : $*Int
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@cc(objc_method) @thin (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[OBJ_REF]]
  // CHECK-NEXT: [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [[GETTER]]([[OBJ_REF]]) : $@cc(objc_method) @thin (Int, @opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[GETTER_WITH_SELF]]([[I]]) : $@callee_owned (Int) -> Int
  // CHECK-NEXT: [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[RESULT]] to [[RESULTTEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]{{.*}}Some
  // CHECK-NEXT: br bb3
  var x: Int = obj[i]!
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup16opt_to_subscript
func opt_to_subscript(var obj: AnyObject, var i: Int) {
  // CHECK-NEXT: bb0([[OBJ:%[0-9]+]] : $AnyObject, [[I:%[0-9]+]] : $Int):
  // CHECK-NEXT: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[I_BOX:%[0-9]+]] = alloc_box $Int
  // CHECK-NEXT: store [[I]] to [[I_BOX]]#1 : $*Int
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[OBJ_REF:%[0-9]+]] = open_existential_ref [[OBJ]] : $AnyObject to $@opened({{.*}}) AnyObject
  // CHECK-NEXT: [[I:%[0-9]+]] = load [[I_BOX]]#1 : $*Int
  // CHECK-NEXT: [[OPTTEMP:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ_REF]] : $@opened({{.*}}) AnyObject, #X.subscript!getter.1.foreign, bb1, bb2

  // CHECK: bb1([[GETTER:%[0-9]+]] : $@cc(objc_method) @thin (Int, @opened({{.*}}) AnyObject) -> Int):
  // CHECK-NEXT: strong_retain [[OBJ_REF]]
  // CHECK-NEXT: [[GETTER_WITH_SELF:%[0-9]+]] = partial_apply [[GETTER]]([[OBJ_REF]]) : $@cc(objc_method) @thin (Int, @opened({{.*}}) AnyObject) -> Int
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[GETTER_WITH_SELF]]([[I]]) : $@callee_owned (Int) -> Int
  // CHECK-NEXT: [[RESULTTEMP:%.*]] = init_enum_data_addr [[OPTTEMP]]
  // CHECK-NEXT: store [[RESULT]] to [[RESULTTEMP]]
  // CHECK-NEXT: inject_enum_addr [[OPTTEMP]]
  // CHECK-NEXT: br bb3
  obj[i]
}

// CHECK-LABEL: sil hidden @_TF14dynamic_lookup8downcast
func downcast(var obj: AnyObject) -> X {
  // CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
  // CHECK-NEXT: [[OBJ_BOX:%[0-9]+]] = alloc_box $AnyObject
  // CHECK-NEXT: store [[OBJ]] to [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = load [[OBJ_BOX]]#1 : $*AnyObject
  // CHECK-NEXT: strong_retain [[OBJ]] : $AnyObject
  // CHECK-NEXT: [[X:%[0-9]+]] = unconditional_checked_cast [[OBJ]] : $AnyObject to $X
  // CHECK-NEXT: strong_release [[OBJ_BOX]]#0 : $Builtin.NativeObject
  // CHECK-NEXT: return [[X]] : $X
  return obj as! X
}
