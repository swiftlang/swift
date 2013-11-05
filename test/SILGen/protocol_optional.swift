// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

@class_protocol @objc protocol P1 {
  @optional def method(x: Int)

  @optional var prop: Int

  @optional subscript (i: Int) -> Int
}

// CHECK-LABEL: sil @_T17protocol_optional21optionalMethodGenericUSo2P1__FT1tQ__T_ : $@thin <T : P1> (t: T) -> ()
def optionalMethodGeneric<T : P1>(t : T) {
  // CHECK-NEXT: bb0([[T:%[0-9]+]] : $T):
  // CHECK-NEXT: [[TBOX:%[0-9]+]] = alloc_box $T
  // CHECK-NEXT: store [[T]] to [[TBOX]]#1 : $*T
  // CHECK-NEXT: [[OPT_BOX:%[0-9]+]] = alloc_box $Optional<(x: Int64) -> ()>
  // CHECK-NEXT: [[T:%[0-9]+]] = load [[TBOX]]#1 : $*T
  // CHECK-NEXT: strong_retain [[T]] : $T
  // CHECK-NEXT: [[EXIST_REF:%[0-9]+]] = init_existential_ref [[T]] : $T, $DynamicLookup
  // CHECK-NEXT: [[EXIST_REF_PROJ:%[0-9]+]] = project_existential_ref [[EXIST_REF]] : $DynamicLookup to $@sil_self DynamicLookup
  // CHECK-NEXT: [[OBJ_PTR:%[0-9]+]] = ref_to_object_pointer [[EXIST_REF_PROJ]] : $@sil_self DynamicLookup to $Builtin.ObjCPointer
  // CHECK-NEXT: dynamic_method_br [[OBJ_PTR]] : $Builtin.ObjCPointer, #P1.method!1.foreign
  var methodRef = t.method
}

// CHECK-LABEL: sil @_T17protocol_optional23optionalPropertyGenericUSo2P1__FT1tQ__T_ : $@thin <T : P1> (t: T) -> ()
def optionalPropertyGeneric<T : P1>(t : T) {
  // CHECK-NEXT: bb0([[T:%[0-9]+]] : $T):
  // CHECK-NEXT: [[TBOX:%[0-9]+]] = alloc_box $T
  // CHECK-NEXT: store [[T]] to [[TBOX]]#1 : $*T
  // CHECK-NEXT: [[OPT_BOX:%[0-9]+]] = alloc_box $Optional<Int64>
  // CHECK-NEXT: [[T:%[0-9]+]] = load [[TBOX]]#1 : $*T
  // CHECK-NEXT: strong_retain [[T]] : $T
  // CHECK-NEXT: [[EXIST_REF:%[0-9]+]] = init_existential_ref [[T]] : $T, $DynamicLookup
  // CHECK-NEXT: [[EXIST_REF_PROJ:%[0-9]+]] = project_existential_ref [[EXIST_REF]] : $DynamicLookup to $@sil_self DynamicLookup
  // CHECK-NEXT: [[OBJ:%[0-9]+]] = ref_to_object_pointer [[EXIST_REF_PROJ]] : $@sil_self DynamicLookup to $Builtin.ObjCPointer
  // CHECK-NEXT: dynamic_method_br [[OBJ]] : $Builtin.ObjCPointer, #P1.prop!getter.1.foreign
  var propertyRef = t.prop
}
