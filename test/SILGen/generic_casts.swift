// RUN: %swift -emit-silgen %s | FileCheck %s

@class_protocol protocol ClassBound {}
protocol NotClassBound {}

class C : ClassBound, NotClassBound {}
struct S : NotClassBound {}
struct Unloadable : NotClassBound { var x : NotClassBound }

// CHECK-LABEL: sil  @_TF13generic_casts36opaque_archetype_to_opaque_archetype{{.*}}
func opaque_archetype_to_opaque_archetype
<T:NotClassBound, U>(t:T) -> U {
  return (t as U)!
  // CHECK: bb0([[RET:%.*]] : $*U, {{%.*}}: $*T):
  // CHECK:   [[DOWNCAST:%.*]] = unconditional_checked_cast archetype_to_archetype {{%.*}} : {{.*}} to $*U
  // CHECK:   copy_addr [take] [[DOWNCAST]] to [initialization] [[RET]]
}

// CHECK-LABEL: sil  @_TF13generic_casts36opaque_archetype_is_opaque_archetype{{.*}}
func opaque_archetype_is_opaque_archetype
<T:NotClassBound, U>(t:T) -> Bool {
  return t is U
  // CHECK:   checked_cast_br archetype_to_archetype [[VAL:%.*]] : {{.*}} to $*U, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[YES]]({{%.*}}):
  // CHECK:   [[Y:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   br [[CONT:bb[0-9]+]]([[Y]] : $Builtin.Int1)
  // CHECK: [[NO]]:
  // CHECK:   [[N:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   br [[CONT]]([[N]] : $Builtin.Int1)
  // CHECK: [[CONT]]([[I1:%.*]] : $Builtin.Int1):
  // -- apply the _getBool library fn
  // CHECK-NEXT:  function_ref Swift._getBool
  // CHECK-NEXT:  [[GETBOOL:%.*]] = function_ref @_TFSs8_getBoolFBi1_Sb :
  // CHECK-NEXT:  [[RES:%.*]] = apply [transparent] [[GETBOOL]]([[I1]])
  // -- we don't consume the checked value
  // CHECK:   destroy_addr [[VAL]] : $*T
  // CHECK:   return [[RES]] : $Bool
}

// CHECK-LABEL: sil  @_TF13generic_casts35opaque_archetype_to_class_archetype{{.*}}
func opaque_archetype_to_class_archetype
<T:NotClassBound, U:ClassBound> (t:T) -> U {
  return (t as U)!
  // CHECK: [[DOWNCAST_ADDR:%.*]] = unconditional_checked_cast archetype_to_archetype {{%.*}} : {{.*}} to $*U
  // CHECK: [[DOWNCAST:%.*]] = load [[DOWNCAST_ADDR]] : $*U
  // CHECK: return [[DOWNCAST]] : $U
}

// CHECK-LABEL: sil  @_TF13generic_casts35opaque_archetype_is_class_archetype{{.*}}
func opaque_archetype_is_class_archetype
<T:NotClassBound, U:ClassBound> (t:T) -> Bool {
  return t is U
  // CHECK: copy_addr {{.*}} : $*T
  // CHECK: checked_cast_br archetype_to_archetype [[VAL:%.*]] : {{.*}} to $*U
}

// CHECK-LABEL: sil  @_TF13generic_casts34class_archetype_to_class_archetype{{.*}}
func class_archetype_to_class_archetype
<T:ClassBound, U:ClassBound>(t:T) -> U {
  return (t as U)!
  // CHECK: [[DOWNCAST:%.*]] = unconditional_checked_cast archetype_to_archetype {{%.*}} : {{.*}} to $U
  // CHECK: return [[DOWNCAST]] : $U
}

// CHECK-LABEL: sil  @_TF13generic_casts34class_archetype_is_class_archetype{{.*}}
func class_archetype_is_class_archetype
<T:ClassBound, U:ClassBound>(t:T) -> Bool {
  return t is U
  // CHECK: checked_cast_br archetype_to_archetype [[VAL:%.*]] : {{.*}} to $U
}

// CHECK-LABEL: sil  @_TF13generic_casts38opaque_archetype_to_addr_only_concrete{{.*}}
func opaque_archetype_to_addr_only_concrete
<T:NotClassBound> (t:T) -> Unloadable {
  return (t as Unloadable)!
  // CHECK: bb0([[RET:%.*]] : $*Unloadable, {{%.*}}: $*T):
  // CHECK:   [[DOWNCAST:%.*]] = unconditional_checked_cast archetype_to_concrete {{%.*}} : {{.*}} to $*Unloadable
  // CHECK:   copy_addr [take] [[DOWNCAST]] to [initialization] [[RET]]
}

// CHECK-LABEL: sil  @_TF13generic_casts38opaque_archetype_is_addr_only_concrete{{.*}}
func opaque_archetype_is_addr_only_concrete
<T:NotClassBound> (t:T) -> Bool {
  return t is Unloadable
  // CHECK: checked_cast_br archetype_to_concrete [[VAL:%.*]] : {{.*}} to $*Unloadable
}

// CHECK-LABEL: sil  @_TF13generic_casts37opaque_archetype_to_loadable_concrete{{.*}}
func opaque_archetype_to_loadable_concrete
<T:NotClassBound>(t:T) -> S {
  return (t as S)!
  // CHECK: [[DOWNCAST_ADDR:%.*]] = unconditional_checked_cast archetype_to_concrete {{%.*}} to $*S
  // CHECK: [[DOWNCAST:%.*]] = load [[DOWNCAST_ADDR]] : $*S
  // CHECK: return [[DOWNCAST]] : $S
}

// CHECK-LABEL: sil  @_TF13generic_casts37opaque_archetype_is_loadable_concrete{{.*}}
func opaque_archetype_is_loadable_concrete
<T:NotClassBound>(t:T) -> Bool {
  return t is S
  // CHECK: checked_cast_br archetype_to_concrete {{%.*}} to $*S
}

// CHECK-LABEL: sil  @_TF13generic_casts24class_archetype_to_class{{.*}}
func class_archetype_to_class
<T:ClassBound>(t:T) -> C {
  return (t as C)!
  // CHECK: [[DOWNCAST:%.*]] = unconditional_checked_cast archetype_to_concrete {{%.*}} to $C
  // CHECK: return [[DOWNCAST]] : $C
}

// CHECK-LABEL: sil  @_TF13generic_casts24class_archetype_is_class{{.*}}
func class_archetype_is_class
<T:ClassBound>(t:T) -> Bool {
  return t is C
  // CHECK: checked_cast_br archetype_to_concrete {{%.*}} to $C
}

// CHECK-LABEL: sil  @_TF13generic_casts38opaque_existential_to_opaque_archetype{{.*}}
func opaque_existential_to_opaque_archetype
<T:NotClassBound>(p:NotClassBound) -> T {
  return (p as T)!
  // CHECK: bb0([[RET:%.*]] : $*T, {{%.*}}: $*NotClassBound):
  // CHECK:   [[DOWNCAST:%.*]] = unconditional_checked_cast existential_to_archetype [[CONTAINER:%.*]] : {{.*}} to $*T
  // CHECK:   copy_addr [take] [[DOWNCAST]] to [initialization] [[RET]]
  // CHECK:   deinit_existential [[CONTAINER]]
}

// CHECK-LABEL: sil  @_TF13generic_casts38opaque_existential_is_opaque_archetype{{.*}}
func opaque_existential_is_opaque_archetype
<T:NotClassBound>(p:NotClassBound) -> Bool {
  return p is T
  // CHECK:   checked_cast_br existential_to_archetype [[CONTAINER:%.*]] : {{.*}} to $*T
}

// CHECK-LABEL: sil  @_TF13generic_casts37opaque_existential_to_class_archetype{{.*}}
func opaque_existential_to_class_archetype
<T:ClassBound>(p:NotClassBound) -> T {
  return (p as T)!
  // CHECK: [[DOWNCAST_ADDR:%.*]] = unconditional_checked_cast existential_to_archetype {{%.*}} to $*T
  // CHECK: [[DOWNCAST:%.*]] = load [[DOWNCAST_ADDR]] : $*T
  // CHECK: return [[DOWNCAST]] : $T
}

// CHECK-LABEL: sil  @_TF13generic_casts37opaque_existential_is_class_archetype{{.*}}
func opaque_existential_is_class_archetype
<T:ClassBound>(p:NotClassBound) -> Bool {
  return p is T
  // CHECK: checked_cast_br existential_to_archetype {{%.*}} to $*T
}

// CHECK-LABEL: sil  @_TF13generic_casts36class_existential_to_class_archetype{{.*}}
func class_existential_to_class_archetype
<T:ClassBound>(p:ClassBound) -> T {
  return (p as T)!
  // CHECK: [[DOWNCAST:%.*]] = unconditional_checked_cast existential_to_archetype {{%.*}} to $T
  // CHECK: return [[DOWNCAST]] : $T
}

// CHECK-LABEL: sil  @_TF13generic_casts36class_existential_is_class_archetype{{.*}}
func class_existential_is_class_archetype
<T:ClassBound>(p:ClassBound) -> Bool {
  return p is T
  // CHECK: checked_cast_br existential_to_archetype {{%.*}} to $T
}

// CHECK-LABEL: sil  @_TF13generic_casts40opaque_existential_to_addr_only_concrete{{.*}}
func opaque_existential_to_addr_only_concrete(p: NotClassBound) -> Unloadable {
  return (p as Unloadable)!
  // CHECK: bb0([[RET:%.*]] : $*Unloadable, {{%.*}}: $*NotClassBound):
  // CHECK:   [[DOWNCAST:%.*]] = unconditional_checked_cast existential_to_concrete {{%.*}} to $*Unloadable
  // CHECK:   copy_addr [take] [[DOWNCAST]] to [initialization] [[RET]]
}

// CHECK-LABEL: sil  @_TF13generic_casts40opaque_existential_is_addr_only_concrete{{.*}}
func opaque_existential_is_addr_only_concrete(p: NotClassBound) -> Bool {
  return p is Unloadable
  // CHECK:   checked_cast_br existential_to_concrete {{%.*}} to $*Unloadable
}

// CHECK-LABEL: sil  @_TF13generic_casts39opaque_existential_to_loadable_concrete{{.*}}
func opaque_existential_to_loadable_concrete(p: NotClassBound) -> S {
  return (p as S)!
  // CHECK: [[DOWNCAST_ADDR:%.*]] = unconditional_checked_cast existential_to_concrete {{%.*}} to $*S
  // CHECK: [[DOWNCAST:%.*]] = load [[DOWNCAST_ADDR]] : $*S
  // CHECK: return [[DOWNCAST]] : $S
}

// CHECK-LABEL: sil  @_TF13generic_casts39opaque_existential_is_loadable_concrete{{.*}}
func opaque_existential_is_loadable_concrete(p: NotClassBound) -> Bool {
  return p is S
  // CHECK: checked_cast_br existential_to_concrete {{%.*}} to $*S
}

// CHECK-LABEL: sil  @_TF13generic_casts26class_existential_to_class{{.*}}
func class_existential_to_class(p: ClassBound) -> C {
  return (p as C)!
  // CHECK: [[DOWNCAST:%.*]] = unconditional_checked_cast existential_to_concrete {{%.*}} to $C
  // CHECK: return [[DOWNCAST]] : $C
}

// CHECK-LABEL: sil  @_TF13generic_casts26class_existential_is_class{{.*}}
func class_existential_is_class(p: ClassBound) -> Bool {
  return p is C
  // CHECK: checked_cast_br existential_to_concrete {{%.*}} to $C
}

// CHECK-LABEL: sil @_TF13generic_casts27optional_anyobject_to_classFGSqPSs9AnyObject__GSqCS_1C_ 
// CHECK:         checked_cast_br existential_to_concrete {{%.*}} : $AnyObject to $C
func optional_anyobject_to_class(p: AnyObject?) -> C? {
  return p as C
}
