// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol P {}
class C: P {}

struct LoadableStruct {
  var x: C

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT10alwaysFailCS_1C_GSqS0__
  // CHECK:       bb0([[C:%.*]] : $C
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         br [[FAIL:bb[0-9]+]]
  // CHECK:       [[FAIL]]:
  // CHECK:         strong_release [[C]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         [[NIL:%.*]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:         br [[EXIT:bb[0-9]+]]([[NIL]] : $Optional<LoadableStruct>)
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<LoadableStruct>):
  // CHECK:         return [[RESULT]]
  init?(alwaysFail: C) {
    return nil
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT3optSb_GSqS0__
  init?(opt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()

  // CHECK:       bb{{.*}}:
  // CHECK:         br [[FAILURE:bb[0-9]+]]
    if opt {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[SELF:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_OPT:%.*]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, [[SELF]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb.*]]([[SELF_OPT]] : $Optional<LoadableStruct>)

  // CHECK:       [[FAILURE]]:
  // CHECK:         [[NIL:%.*]] = enum $Optional<LoadableStruct>, #Optional.None
  // CHECK:         br [[EXIT]]([[NIL]] : $Optional<LoadableStruct>)
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<LoadableStruct>):
  // CHECK:         return [[RESULT]]

  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT3iuoSb_GSQS0__
  init!(iuo: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()

  // CHECK:       bb{{.*}}:
  // CHECK:         br [[FAILURE:bb[0-9]+]]
    if iuo {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[SELF:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_OPT:%.*]] = enum $ImplicitlyUnwrappedOptional<LoadableStruct>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[SELF]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb.*]]([[SELF_OPT]] : $ImplicitlyUnwrappedOptional<LoadableStruct>)

  // CHECK:       [[FAILURE]]:
  // CHECK:         [[NIL:%.*]] = enum $ImplicitlyUnwrappedOptional<LoadableStruct>, #ImplicitlyUnwrappedOptional.None
  // CHECK:         br [[EXIT]]([[NIL]] : $ImplicitlyUnwrappedOptional<LoadableStruct>)
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $ImplicitlyUnwrappedOptional<LoadableStruct>):
  // CHECK:         return [[RESULT]]
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT15delegatesOptOptSb_GSqS0__
  init?(delegatesOptOpt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FT3optSb_GSqS0__
  // CHECK:         [[DELEGATEE_SELF:%.*]] = apply [[DELEGATEE_INIT]]
    
  // CHECK: = integer_literal $Builtin.Int1, -1
  // CHECK: = integer_literal $Builtin.Int1, 0
  // CHECK: = select_enum {{%.*}} : $Optional<LoadableStruct>, case #Optional.Some!enumelt.1:
  // CHECK: cond_br {{.*}}, [[DOES_HAVE_VALUE:bb[0-9]+]], [[FAILURE:bb[0-9]+]]
  // -- TODO: failure
  // CHECK:       [[DOES_HAVE_VALUE]]:
  // CHECK:         [[DELEGATEE_SELF_VAL:%.*]] = unchecked_enum_data [[DELEGATEE_SELF]] : $Optional<LoadableStruct>, #Optional.Some!enumelt.1
  // CHECK:         assign [[DELEGATEE_SELF_VAL]] to [[SELF_MARKED]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:       [[FAILURE]]:
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         [[NIL:%.*]] = enum $Optional<LoadableStruct>, #Optional.None
  // CHECK:         br [[EXIT:bb.*]]([[NIL]] : $Optional<LoadableStruct>)
    self.init(opt: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT16delegatesNormIUOSb_S0_
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FT3iuoSb_GSQS0__
  // CHECK:         [[DELEGATEE_SELF:%.*]] = apply [[DELEGATEE_INIT]]
  // CHECK:         [[DELEGATEE_SELF_MAT:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<LoadableStruct>
  // CHECK:         store [[DELEGATEE_SELF]] to [[DELEGATEE_SELF_MAT]]
  // CHECK:         [[GET_VALUE_FN:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:         [[TMP:%.*]] = alloc_stack $LoadableStruct
  // CHECK:         apply [[GET_VALUE_FN]]<LoadableStruct>([[TMP]]#1, [[DELEGATEE_SELF_MAT]]#1)
  // CHECK:         [[DELEGATEE_SELF_VAL:%.*]] = load [[TMP]]
  // CHECK:         assign [[DELEGATEE_SELF_VAL]] to [[SELF_MARKED]]
  // CHECK:         dealloc_stack [[TMP]]
  // CHECK:         dealloc_stack [[DELEGATEE_SELF_MAT]]
  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
}

struct AddressOnlyStruct {
  var x: P

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3optSb_GSqS0__
  init?(opt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // CHECK:       bb{{.*}}:
  // CHECK:         br [[FAILURE:bb[0-9]+]]
    if opt {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[DEST_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some
  // CHECK:         copy_addr [[SELF_MARKED]] to [initialization] [[DEST_PAYLOAD]]
  // CHECK:         inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb[0-9]+]]

  // CHECK:       [[FAILURE]]:
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:         br [[EXIT]]
  // CHECK:       [[EXIT]]:
  // CHECK:         return
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3iuoSb_GSQS0__
  init!(iuo: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // CHECK:       bb{{.*}}:
  // CHECK:         br [[FAILURE:bb[0-9]+]]
    if iuo {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[DEST_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*ImplicitlyUnwrappedOptional<AddressOnlyStruct>, #ImplicitlyUnwrappedOptional.Some
  // CHECK:         copy_addr [[SELF_MARKED]] to [initialization] [[DEST_PAYLOAD]]
  // CHECK:         inject_enum_addr %0 : $*ImplicitlyUnwrappedOptional<AddressOnlyStruct>, #ImplicitlyUnwrappedOptional.Some
  // CHECK:         strong_release [[SELF_BOX]]

  // CHECK:       [[FAILURE]]:
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         inject_enum_addr %0 : $*ImplicitlyUnwrappedOptional<AddressOnlyStruct>, #ImplicitlyUnwrappedOptional.None!enumelt
  // CHECK:         br [[EXIT:bb[0-9]+]]
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT15delegatesOptOptSb_GSqS0__
  init?(delegatesOptOpt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3optSb_GSqS0__
  // CHECK:         [[DELEGATEE_SELF:%.*]] = alloc_stack $Optional<AddressOnlyStruct>
  // CHECK:         apply [[DELEGATEE_INIT]]([[DELEGATEE_SELF]]
  // CHECK:         [[HAS_VALUE:%.*]] = select_enum_addr [[DELEGATEE_SELF]]
  // CHECK:         cond_br [[HAS_VALUE]], [[DOES_HAVE_VALUE:bb[0-9]+]], [[DOESNT_HAVE_VALUE:bb[0-9]+]]
  // -- TODO: failure
  // CHECK:       [[DOESNT_HAVE_VALUE]]:
  // CHECK:         dealloc_stack [[DELEGATEE_SELF]]
  // CHECK:         br [[FAILURE:bb[0-9]+]]
  // CHECK:       [[DOES_HAVE_VALUE]]:
  // CHECK:         [[TMP:%.*]] = unchecked_take_enum_data_addr [[DELEGATEE_SELF]]#1 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:         copy_addr [take] [[TMP]] to [[SELF_MARKED]]
  // CHECK:         dealloc_stack [[DELEGATEE_SELF]]
  // CHECK:         strong_release [[SELF_BOX]]
    self.init(opt: true)
  // CHECK:       [[FAILURE]]:
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:         br [[EXIT:bb[0-9]+]]
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT16delegatesNormIUOSb_S0_
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3iuoSb_GSQS0__
  // CHECK:         [[DELEGATEE_SELF:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<AddressOnlyStruct>
  // CHECK:         apply [[DELEGATEE_INIT]]([[DELEGATEE_SELF]]
  // CHECK:         [[GET_VALUE_FN:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:         [[TMP:%.*]] = alloc_stack $AddressOnlyStruct
  // CHECK:         apply [[GET_VALUE_FN]]<AddressOnlyStruct>([[TMP]]#1, [[DELEGATEE_SELF]]#1)
  // CHECK:         copy_addr [take] [[TMP]]#1 to [[SELF_MARKED]]
  // CHECK:         dealloc_stack [[TMP]]
  // CHECK:         dealloc_stack [[DELEGATEE_SELF]]
  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
}

class RootClass {
  var x: C

  init(norm: Bool) {
    x = C()
  }

  init?(alwaysFail: Void) {
    return nil
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers9RootClasscfMS0_FT3optSb_GSqS0__
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself]
  init?(opt: Bool) {
    x = C()
  // CHECK:         cond_br {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]

    if opt {
  // CHECK:       [[YES]]:
  // CHECK:         br [[FAILURE:bb[0-9]+]]
      return nil
    }

  // CHECK:       [[NO]]:
  // CHECK:         [[SOME:%.*]] = enum $Optional<RootClass>, #Optional.Some!enumelt.1, [[SELF_MARKED]]
  // CHECK:         br [[EXIT:bb[0-9]+]]([[SOME]] : $Optional<RootClass>)

  // CHECK:       [[FAILURE]]:
  // CHECK:         strong_release [[SELF_MARKED]]
  // CHECK:         [[NIL:%.*]] = enum $Optional<RootClass>, #Optional.None!enumelt
  // CHECK:         br [[EXIT]]([[NIL]] : $Optional<RootClass>)
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<RootClass>):
  // CHECK:         return [[RESULT]]
  }

  init!(iuo: Bool) {
    x = C()
    if iuo {
      return nil
    }
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers9RootClasscfMS0_FT16delegatesOptNormSb_GSqS0__
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $RootClass
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself]
  // CHECK:         store %1 to [[SELF_MARKED]]
  // CHECK:         [[SELF_TAKEN:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[INIT:%.*]] = class_method [[SELF_TAKEN]] : $RootClass, #RootClass.init
  // CHECK:         [[NEW_SELF:%.*]] = apply [[INIT]]({{.*}}, [[SELF_TAKEN]])
  // CHECK:         store [[NEW_SELF]] to [[SELF_MARKED]]

  // CHECK:         [[RESULT_SELF:%.*]] = load [[SELF_MARKED]]
  // CHECK:         strong_retain [[RESULT_SELF]]
  // CHECK:         [[SOME:%.*]] = enum $Optional<RootClass>, #Optional.Some!enumelt.1, [[RESULT_SELF]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb[0-9]+]]([[SOME]] : $Optional<RootClass>)
  
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<RootClass>):
  // CHECK:         return [[RESULT]]
  convenience init?(delegatesOptNorm: Bool) {
    self.init(norm: true)
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers9RootClasscfMS0_FT15delegatesOptOptSb_GSqS0__
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $RootClass
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself]
  // CHECK:         store %1 to [[SELF_MARKED]]
  // CHECK:         [[SELF_TAKEN:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[INIT:%.*]] = class_method [[SELF_TAKEN]] : $RootClass, #RootClass.init
  // CHECK:         [[NEW_SELF_OPT:%.*]] = apply [[INIT]]({{.*}}, [[SELF_TAKEN]])
  // CHECK:         [[HAS_VALUE1:%.*]] = select_enum [[NEW_SELF_OPT]]
  // CHECK:         cond_br [[HAS_VALUE1]], [[HAS_VALUE:bb[0-9]+]], [[FAILURE:bb[0-9]+]]

  // CHECK:       [[HAS_VALUE]]:
  // CHECK:         [[NEW_SELF:%.*]] = unchecked_enum_data [[NEW_SELF_OPT]]
  // CHECK:         store [[NEW_SELF]] to [[SELF_MARKED]]

  // CHECK:         [[SELF_RESULT:%.*]] = load [[SELF_MARKED]]
  // CHECK:         strong_retain [[SELF_RESULT]]
  // CHECK:         [[SOME:%.*]] = enum $Optional<RootClass>, #Optional.Some!enumelt.1, [[SELF_RESULT]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb[0-9]+]]([[SOME]] : $Optional<RootClass>)

  // CHECK:       [[FAILURE]]:
  // CHECK:          strong_release [[SELF_BOX]]#0 : $Builtin.NativeObject
  // CHECK:         [[NIL:%.*]] = enum $Optional<RootClass>, #Optional.None!enumelt
  // CHECK:         br [[EXIT]]([[NIL]] : $Optional<RootClass>)
  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<RootClass>):
  // CHECK:         return [[RESULT]]

  convenience init?(delegatesOptOpt: Bool) {
    self.init(opt: true)
  }

  convenience init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
}

class SubClass: RootClass {
  var y: C

  init(normInheritNorm: Bool) {
    y = C()
    super.init(norm: true)
  }

  init(normInheritIUO: Bool) {
    y = C()
    super.init(iuo: true)
  }

  init?(optInheritNorm: Bool) {
    y = C()
    super.init(norm: true)
    if optInheritNorm {
      return nil
    }
  }

  init?(optInheritOpt: Bool) {
    y = C()
    super.init(opt: true)
    if optInheritOpt {
      return nil
    }
  }
}
