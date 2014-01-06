// RUN: %swift -emit-silgen %s | FileCheck %s

struct X { }

// Initializer delegation within a struct.
struct S {
  // CHECK-DAG: sil @_TFV19init_ref_delegation1SCfMS0_FT_S0_ : $@thin (@thin S.metatype) -> S {
  init() {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin S.metatype):
    // CHECK-NEXT:   [[SELF_BOX:%[0-9]+]] = alloc_box $S
    // CHECK-NEXT:   [[SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_BOX]]#1 : $*S
    // CHECK-NEXT:   [[VAR_3:%[0-9]+]] = tuple ()
    // CHECK-NEXT:   [[SELF_META:%[0-9]+]] = metatype $@thin S.metatype
    
    // CHECK:   [[S_DELEG_INIT:%[0-9]+]] = function_ref @_TFV19init_ref_delegation1SCfMS0_FT1xVS_1X_S0_ : $@thin (X, @thin S.metatype) -> S
    
    // CHECK:   [[X_CTOR:%[0-9]+]] = function_ref @_TFV19init_ref_delegation1XCfMS0_FT_S0_ : $@thin (@thin X.metatype) -> X
    // CHECK-NEXT:   [[X_META:%[0-9]+]] = metatype $@thin X.metatype
    // CHECK-NEXT:   [[X:%[0-9]+]] = apply [[X_CTOR]]([[X_META]]) : $@thin (@thin X.metatype) -> X
    // CHECK-NEXT:   [[REPLACEMENT_SELF:%[0-9]+]] = apply [[S_DELEG_INIT]]([[X]], [[SELF_META]]) : $@thin (X, @thin S.metatype) -> S
    self.init(X())
    // CHECK-NEXT:   assign [[REPLACEMENT_SELF]] to [[SELF]] : $*S
    // CHECK-NEXT:   [[SELF_BOX1:%[0-9]+]] = load [[SELF]] : $*S
    // CHECK-NEXT:   strong_release [[SELF_BOX]]#0 : $Builtin.ObjectPointer
    // CHECK-NEXT:   return [[SELF_BOX1]] : $S
  }

  init(x: X) { }
}

// Initializer delegation within an enum
enum E {
  // CHECK-DAG: sil @_TFO19init_ref_delegation1ECfMS0_FT_S0_ : $@thin (@thin E.metatype) -> E
  init() {
    // CHECK: bb0([[E_META:%[0-9]+]] : $@thin E.metatype):
    // CHECK:   [[E_BOX:%[0-9]+]] = alloc_box $E
    // CHECK:   [[E_SELF:%[0-9]+]] = mark_uninitialized [rootself] [[E_BOX]]#1 : $*E
    // CHECK:   [[VAR_3:%[0-9]+]] = tuple ()
    // CHECK:   [[E_META:%[0-9]+]] = metatype $@thin E.metatype

    // CHECK:   [[X_INIT:%[0-9]+]] = function_ref @_TFO19init_ref_delegation1ECfMS0_FT1xVS_1X_S0_ : $@thin (X, @thin E.metatype) -> E

    // CHECK:   [[E_DELEG_INIT:%[0-9]+]] = function_ref @_TFV19init_ref_delegation1XCfMS0_FT_S0_ : $@thin (@thin X.metatype) -> X
    // CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.metatype
    // CHECK:   [[X:%[0-9]+]] = apply [[E_DELEG_INIT]]([[X_META]]) : $@thin (@thin X.metatype) -> X
    // CHECK:   [[S:%[0-9]+]] = apply [[X_INIT]]([[X]], [[E_META]]) : $@thin (X, @thin E.metatype) -> E
    // CHECK:   assign [[S:%[0-9]+]] to [[E_SELF]] : $*E
    // CHECK:   [[E_BOX1:%[0-9]+]] = load [[E_SELF]] : $*E
    self.init(X())
    // CHECK:   strong_release [[E_BOX:%[0-9]+]]#0 : $Builtin.ObjectPointer
    // CHECK:   return [[E_BOX1:%[0-9]+]] : $E
  }

  init(x: X) { }
}

// Initializer delegation to a generic initializer
struct S2 {
  // CHECK-DAG: sil @_TFV19init_ref_delegation2S2CfMS0_FT_S0_ : $@thin (@thin S2.metatype) -> S2
  init() {
    // CHECK: bb0([[S2_META:%[0-9]+]] : $@thin S2.metatype):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $S2
    // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_BOX]]#1 : $*S2
    // CHECK:   [[VAR_3:%[0-9]+]] = tuple ()
    // CHECK:   [[S2_META:%[0-9]+]] = metatype $@thin S2.metatype

    // CHECK:   [[S2_DELEG_INIT:%[0-9]+]] = function_ref @_TFV19init_ref_delegation2S2CfMS0_U__FT1tQ__S0_ : $@thin <$T_0_0> (@in $T_0_0, @thin S2.metatype) -> S2

    // CHECK:   [[X_INIT:%[0-9]+]] = function_ref @_TFV19init_ref_delegation1XCfMS0_FT_S0_ : $@thin (@thin X.metatype) -> X
    // CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.metatype
    // CHECK:   [[X:%[0-9]+]] = apply [[X_INIT]]([[X_META]]) : $@thin (@thin X.metatype) -> X
    // CHECK:   [[X_BOX:%[0-9]+]] = alloc_stack $X
    // CHECK:   store [[X]] to [[X_BOX]]#1 : $*X
    // CHECK:   [[SELF_BOX1:%[0-9]+]] = apply [[S2_DELEG_INIT]]<T = X>([[X_BOX]]#1, [[S2_META]]) : $@thin <$T_0_0> (@in $T_0_0, @thin S2.metatype) -> S2
    // CHECK:   assign [[SELF_BOX1]] to [[SELF]] : $*S2
    // CHECK:   dealloc_stack [[X_BOX]]#0 : $*@local_storage X
    // CHECK:   [[SELF_BOX4:%[0-9]+]] = load [[SELF]] : $*S2
    self.init(X())
    // CHECK:   strong_release [[SELF_BOX]]#0 : $Builtin.ObjectPointer
    // CHECK:   return [[SELF_BOX4]] : $S2
  }

  init<T>(t: T) { }
}
