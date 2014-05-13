// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

@class_protocol @objc protocol P1 {
  @optional func method(x: Int)

  @optional var prop: Int { get }

  @optional subscript (i: Int) -> Int { get }
}

// CHECK-LABEL: sil @{{.*}}optionalMethodGeneric{{.*}} : $@thin <T where T : P1> (@owned T) -> ()
func optionalMethodGeneric<T : P1>(var #t : T) {
  // CHECK-NEXT: bb0([[T:%[0-9]+]] : $T):
  // CHECK-NEXT: [[TBOX:%[0-9]+]] = alloc_box $T
  // CHECK-NEXT: store [[T]] to [[TBOX]]#1 : $*T
  // CHECK-NEXT: [[OPT_BOX:%[0-9]+]] = alloc_box $Optional<Int -> ()>
  // CHECK-NEXT: [[T:%[0-9]+]] = load [[TBOX]]#1 : $*T
  // CHECK-NEXT: strong_retain [[T]] : $T
  // CHECK-NEXT: [[EXIST_REF:%[0-9]+]] = init_existential_ref [[T]] : $T, $AnyObject
  // CHECK-NEXT: [[EXIST_REF_PROJ:%[0-9]+]] = project_existential_ref [[EXIST_REF]] : $AnyObject to $@sil_self AnyObject
  // CHECK-NEXT: [[OBJ_PTR:%[0-9]+]] = unchecked_ref_cast [[EXIST_REF_PROJ]] : $@sil_self AnyObject to $Builtin.UnknownObject
  // CHECK-NEXT: alloc_stack $Optional<Int -> ()>
  // CHECK-NEXT: dynamic_method_br [[OBJ_PTR]] : $Builtin.UnknownObject, #P1.method!1.foreign
  var methodRef = t.method
}

// CHECK-LABEL: sil @_TF17protocol_optional23optionalPropertyGenericUS_2P1__FT1tQ__T_ : $@thin <T where T : P1> (@owned T) -> ()
func optionalPropertyGeneric<T : P1>(var #t : T) {
  // CHECK-NEXT: bb0([[T:%[0-9]+]] : $T):
  // CHECK-NEXT: [[TBOX:%[0-9]+]] = alloc_box $T
  // CHECK-NEXT: store [[T]] to [[TBOX]]#1 : $*T
  // CHECK-NEXT: [[OPT_BOX:%[0-9]+]] = alloc_box $Optional<Int>
  // CHECK-NEXT: [[T:%[0-9]+]] = load [[TBOX]]#1 : $*T
  // CHECK-NEXT: strong_retain [[T]] : $T
  // CHECK-NEXT: [[EXIST_REF:%[0-9]+]] = init_existential_ref [[T]] : $T, $AnyObject
  // CHECK-NEXT: [[EXIST_REF_PROJ:%[0-9]+]] = project_existential_ref [[EXIST_REF]] : $AnyObject to $@sil_self AnyObject
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = unchecked_ref_cast [[EXIST_REF_PROJ]] : $@sil_self AnyObject to $Builtin.UnknownObject
  // CHECK-NEXT: alloc_stack $Optional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ]] : $Builtin.UnknownObject, #P1.prop!getter.1.foreign
  var propertyRef = t.prop
}

// CHECK-LABEL: sil @_TF17protocol_optional24optionalSubscriptGenericUS_2P1__FT1tQ__T_ : $@thin <T where T : P1> (@owned T) -> ()
func optionalSubscriptGeneric<T : P1>(var #t : T) {
  // CHECK-NEXT: bb0([[T:%[0-9]+]] : $T):
  // CHECK-NEXT: [[TBOX:%[0-9]+]] = alloc_box $T
  // CHECK-NEXT: store [[T]] to [[TBOX]]#1 : $*T
  // CHECK-NEXT: [[OPT_BOX:%[0-9]+]] = alloc_box $Optional<Int>
  // CHECK-NEXT: [[T:%[0-9]+]] = load [[TBOX]]#1 : $*T
  // CHECK-NEXT: strong_retain [[T]] : $T
  // CHECK-NEXT: [[EXIST_REF:%[0-9]+]] = init_existential_ref [[T]] : $T, $AnyObject
  // CHECK-NEXT: [[EXIST_REF_PROJ:%[0-9]+]] = project_existential_ref [[EXIST_REF]] : $AnyObject to $@sil_self AnyObject
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = unchecked_ref_cast [[EXIST_REF_PROJ]] : $@sil_self AnyObject to $Builtin.UnknownObject
  // CHECK: [[INTCONV:%[0-9]+]] = function_ref @_TFSi33_convertFromBuiltinIntegerLiteral
  // CHECK-NEXT: [[INT64:%[0-9]+]] = metatype $@thin Int.Type
  // CHECK-NEXT: [[FIVELIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 5
  // CHECK-NEXT: [[FIVE:%[0-9]+]] = apply [transparent] [[INTCONV]]([[FIVELIT]], [[INT64]]) : $@thin (Builtin.Int2048, @thin Int.Type) -> Int
  // CHECK-NEXT: alloc_stack $Optional<Int>
  // CHECK-NEXT: dynamic_method_br [[OBJ]] : $Builtin.UnknownObject, #P1.subscript!getter.1.foreign
  var subscriptRef = t[5]
}
