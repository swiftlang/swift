// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
// RUN: %target-swift-frontend -emit-sil %s > /dev/null

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
  // CHECK:         [[DELEGATEE_SELF_MAT:%[0-9]+]] = alloc_stack $ImplicitlyUnwrappedOptional<LoadableStruct>
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FT3iuoSb_GSQS0__
  // CHECK:         [[DELEGATEE_SELF:%.*]] = apply [[DELEGATEE_INIT]]
  // CHECK:         [[GET_VALUE_FN:%.*]] = function_ref @_TFs36_getImplicitlyUnwrappedOptionalValueurFGSQq__q_
  // CHECK:         [[TMP:%.*]] = alloc_stack $LoadableStruct
  // CHECK:         apply [[GET_VALUE_FN]]<LoadableStruct>([[TMP]]#1, [[DELEGATEE_SELF_MAT]]#1)
  // CHECK:         [[DELEGATEE_SELF_VAL:%.*]] = load [[TMP]]
  // CHECK:         assign [[DELEGATEE_SELF_VAL]] to [[SELF_MARKED]]
  // CHECK:         dealloc_stack [[TMP]]
  // CHECK:         dealloc_stack [[DELEGATEE_SELF_MAT]]
  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT14delegateToFail
  init(delegateToFail: C) {
    // CHECK: bb0([[C:%[0-9]+]] : $C, [[SELF_META:%[0-9]+]] : $@thin LoadableStruct.Type):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $LoadableStruct
    // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*LoadableStruct
    // CHECK:   function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FT10alwaysFailCS_1C_GSqS0__
    // CHECK:   function_ref @_TFs17_getOptionalValueurFGSqq__q_ : $@convention(thin) <τ_0_0> (@out τ_0_0, @in Optional<τ_0_0>) -> ()
    // CHECK: ret
    self.init(alwaysFail: delegateToFail)!
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
  // CHECK:         [[DELEGATEE_SELF:%.*]] = alloc_stack $ImplicitlyUnwrappedOptional<AddressOnlyStruct>
  // CHECK:         [[DELEGATEE_INIT:%.*]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3iuoSb_GSQS0__
  // CHECK:         apply [[DELEGATEE_INIT]]([[DELEGATEE_SELF]]
  // CHECK:         [[GET_VALUE_FN:%.*]] = function_ref @_TFs36_getImplicitlyUnwrappedOptionalValueurFGSQq__q_
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
    fatalError()
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
  // CHECK:          strong_release [[SELF_BOX]]#0 : $@box RootClass
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

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers9RootClasscfMS0_FT16delegatesOptNormSb3optSb_GSqS0__
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $RootClass
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself]
  // CHECK:         store %2 to [[SELF_MARKED]]
  // CHECK:         cond_br {{.*}}, [[FAILURE:bb[0-9]+]], [[HAS_VALUE:bb[0-9]+]]

  // CHECK:       [[FAILURE]]:
  // CHECK:         br [[NO_VALUE:bb[0-9]+]]

  // CHECK:       [[HAS_VALUE]]:
  // CHECK:         [[SELF_TAKEN:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_NULL:%.*]] = null_class $RootClass
  // CHECK:         store [[SELF_NULL]] to [[SELF_MARKED]]
  // CHECK:         [[INIT:%.*]] = class_method [[SELF_TAKEN]] : $RootClass, #RootClass.init
  // CHECK:         [[NEW_SELF:%.*]] = apply [[INIT]]({{.*}}, [[SELF_TAKEN]])
  // CHECK:         store [[NEW_SELF]] to [[SELF_MARKED]]
  // CHECK:         [[SELF_RESULT:%.*]] = load [[SELF_MARKED]]
  // CHECK:         strong_retain [[SELF_RESULT]]
  // CHECK:         [[SOME:%.*]] = enum $Optional<RootClass>, #Optional.Some!enumelt.1, [[SELF_RESULT]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb[0-9]+]]([[SOME]] : $Optional<RootClass>)

  // CHECK:       [[NO_VALUE]]:
  // CHECK:         strong_release [[SELF_BOX]]#0 : $@box RootClass
  // CHECK:         [[NIL:%.*]] = enum $Optional<RootClass>, #Optional.None!enumelt
  // CHECK:         br [[EXIT]]([[NIL]] : $Optional<RootClass>)

  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<RootClass>):
  // CHECK:         return [[RESULT]]

  convenience init?(delegatesOptNorm: Bool, opt: Bool) {
    if (opt) {
      return nil
    }
    self.init(norm: true)
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers9RootClasscfMS0_FT15delegatesOptOptSb3optSb_GSqS0__
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $RootClass
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [delegatingself]
  // CHECK:         store %2 to [[SELF_MARKED]]
  // CHECK:         cond_br {{.*}}, [[FAILURE:bb[0-9]+]], [[SUCCESS:bb[0-9]+]]

  // CHECK:       [[FAILURE]]:
  // CHECK:         br [[NO_VALUE:bb[0-9]+]]

  // CHECK:       [[SUCCESS]]:
  // CHECK:         [[SELF_TAKEN:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_NULL:%.*]] = null_class $RootClass
  // CHECK:         store [[SELF_NULL]] to [[SELF_MARKED]]
  // CHECK:         [[INIT:%.*]] = class_method [[SELF_TAKEN]] : $RootClass, #RootClass.init
  // CHECK:         [[NEW_SELF_OPT:%.*]] = apply [[INIT]]({{.*}}, [[SELF_TAKEN]])
  // CHECK:         [[HAS_VALUE1:%.*]] = select_enum [[NEW_SELF_OPT]]
  // CHECK:         cond_br [[HAS_VALUE1]], [[HAS_VALUE:bb[0-9]+]], [[NO_VALUE:bb[0-9]+]]

  // CHECK:       [[HAS_VALUE]]:
  // CHECK:         [[NEW_SELF:%.*]] = unchecked_enum_data [[NEW_SELF_OPT]]
  // CHECK:         store [[NEW_SELF]] to [[SELF_MARKED]]

  // CHECK:         [[SELF_RESULT:%.*]] = load [[SELF_MARKED]]
  // CHECK:         strong_retain [[SELF_RESULT]]
  // CHECK:         [[SOME:%.*]] = enum $Optional<RootClass>, #Optional.Some!enumelt.1, [[SELF_RESULT]]
  // CHECK:         strong_release [[SELF_BOX]]
  // CHECK:         br [[EXIT:bb[0-9]+]]([[SOME]] : $Optional<RootClass>)

  // CHECK:       [[NO_VALUE]]:
  // CHECK:          strong_release [[SELF_BOX]]#0 : $@box RootClass
  // CHECK:         [[NIL:%.*]] = enum $Optional<RootClass>, #Optional.None!enumelt
  // CHECK:         br [[EXIT]]([[NIL]] : $Optional<RootClass>)

  // CHECK:       [[EXIT]]([[RESULT:%.*]] : $Optional<RootClass>):
  // CHECK:         return [[RESULT]]

  convenience init?(delegatesOptOpt: Bool, opt: Bool) {
    if (opt) {
      return nil
    }
    self.init(opt: true)
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


// <rdar://problem/20941576> SILGen crash: Failable struct init cannot delegate to another failable initializer
struct TrivialFailableInit {
  init?(blah: String) { }
  init?(wibble: String) {
    self.init(blah: wibble)
  }
}


extension LoadableStruct {
  init(error: Bool) throws {
    x = C()
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FzT9propagateSb_S0_
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT5errorSb_S0_
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned LoadableStruct, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $LoadableStruct):
  // CHECK:   assign [[VALUE]] to [[BOX:%.+]] : $*LoadableStruct
  // CHECK:   [[RESULT:%.+]] = load [[BOX]] : $*LoadableStruct
  // CHECK:   retain_value [[RESULT]] : $LoadableStruct
  // CHECK:   return [[RESULT]] : $LoadableStruct
  // CHECK: [[FAILURE]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   throw [[ERROR]] : $ErrorType
  // CHECK: {{^}$}}
  init(propagate: Bool) throws {
    try self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT5forceSb_S0_
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT5errorSb_S0_
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned LoadableStruct, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $LoadableStruct):
  // CHECK:   assign [[VALUE]] to [[BOX:%.+]] : $*LoadableStruct
  // CHECK:   [[RESULT:%.+]] = load [[BOX]] : $*LoadableStruct
  // CHECK:   retain_value [[RESULT]] : $LoadableStruct
  // CHECK:   return [[RESULT]] : $LoadableStruct
  // CHECK: [[FAILURE:[^ ]+]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   builtin "unexpectedError"
  // CHECK: [[CLEANUP]]([[ERROR:%.+]] : $ErrorType):
  // CHECK-NEXT:   br [[FAILURE]]([[ERROR]] : $ErrorType)
  // CHECK: {{^}$}}
  init(force: Bool) {
    try! self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT5maybeSb_GSqS0__
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT5errorSb_S0_
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned LoadableStruct, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $LoadableStruct):
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, [[VALUE]] : $LoadableStruct
  // CHECK:   br [[CHECK:[^ ]+]]([[WRAPPED]] : $Optional<LoadableStruct>)
  // CHECK: [[CHECK]]([[RESULT:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<LoadableStruct>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   = unchecked_enum_data [[RESULT]] : $Optional<LoadableStruct>, #Optional.Some!enumelt.1
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, {{%.+}} : $LoadableStruct
  // CHECK:   br [[RETURN:[^ ]+]]([[FINAL_RESULT]] : $Optional<LoadableStruct>)
  // CHECK: [[FAILURE]]:
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]([[FINAL_RESULT]] : $Optional<LoadableStruct>)
  // CHECK: [[RETURN]]([[RET_VAL:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   return [[RET_VAL]] : $Optional<LoadableStruct>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   [[FAIL_VALUE:%.+]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]([[FAIL_VALUE]] : $Optional<LoadableStruct>)
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(maybe: Bool) {
    try? self.init(error: true)
  }

  init?(complex: Bool) throws {
    try self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FzT10propagate2Sb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $LoadableStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*LoadableStruct
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT7complexSb_GSqS0__
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned Optional<LoadableStruct>, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   store [[VALUE]] to [[BOX:%.+]]#1 : $*Optional<LoadableStruct>
  // CHECK:   [[FORCE_FN:%.+]] = function_ref @_TFs17_getOptionalValueurFGSqq__q_
  // CHECK:   [[RESULT_BOX:%.+]] = alloc_stack $LoadableStruct
  // CHECK:   = apply [[FORCE_FN]]<LoadableStruct>([[RESULT_BOX]]#1, [[BOX]]#1)
  // CHECK:   [[RESULT:%.+]] = load [[RESULT_BOX]]#1 : $*LoadableStruct
  // CHECK:   assign [[RESULT]] to [[SELF_BOX_VAL]] : $*LoadableStruct
  // CHECK:   dealloc_stack [[RESULT_BOX]]#0
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   [[FINAL_RESULT:%.+]] = load [[SELF_BOX_VAL]]
  // CHECK:   retain_value [[FINAL_RESULT]] : $LoadableStruct
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   return [[FINAL_RESULT]] : $LoadableStruct
  // CHECK: [[FAILURE]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   throw [[ERROR]] : $ErrorType
  // CHECK: {{^}$}}
  init(propagate2: Bool) throws {
    try self.init(complex: true)!
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT6force2Sb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $LoadableStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*LoadableStruct
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT7complexSb_GSqS0__
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned Optional<LoadableStruct>, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   [[CASE:%.+]] = select_enum [[VALUE]] : $Optional<LoadableStruct>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[VALUE]] : $Optional<LoadableStruct>, #Optional.Some!enumelt.1
  // CHECK:   assign [[UNWRAPPED]] to [[SELF_BOX_VAL]] : $*LoadableStruct
  // CHECK:   [[RESULT:%.+]] = load [[SELF_BOX_VAL]] : $*LoadableStruct
  // CHECK:   retain_value [[RESULT]] : $LoadableStruct
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, [[RESULT]] : $LoadableStruct
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]([[WRAPPED]] : $Optional<LoadableStruct>)
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[NONE_VAL:%.+]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:   br [[RETURN:[^ ]+]]([[NONE_VAL]] : $Optional<LoadableStruct>)
  // CHECK: [[RETURN]]([[RESULT:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   return [[RESULT]] : $Optional<LoadableStruct>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   builtin "unexpectedError"
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(force2: Bool) {
    try! self.init(complex: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers14LoadableStructCfMS0_FT6maybe2Sb_GSqS0__
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers14LoadableStructCfMS0_FzT7complexSb_GSqS0__
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, %1) : $@convention(thin) (Bool, @thin LoadableStruct.Type) -> (@owned Optional<LoadableStruct>, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<Optional<LoadableStruct>>, #Optional.Some!enumelt.1, [[VALUE]] : $Optional<LoadableStruct>
  // CHECK:   br [[CHECK:[^ ]+]]([[WRAPPED]] : $Optional<Optional<LoadableStruct>>)
  // CHECK: [[CHECK]]([[RESULT:%.+]] : $Optional<Optional<LoadableStruct>>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<Optional<LoadableStruct>>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[UNWRAP:[^ ]+]], [[NONE:[^ ]+]]
  // CHECK: [[UNWRAP]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[RESULT]] : $Optional<Optional<LoadableStruct>>, #Optional.Some!enumelt.1
  // CHECK:   br [[CHECK_MERGED:[^ ]+]]([[UNWRAPPED]] : $Optional<LoadableStruct>)
  // CHECK: [[NONE]]:
  // CHECK:   [[NONE_VAL:%.+]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:   br [[CHECK_MERGED]]([[NONE_VAL]] : $Optional<LoadableStruct>)
  // CHECK: [[CHECK_MERGED]]([[RESULT:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<LoadableStruct>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   = unchecked_enum_data [[RESULT]] : $Optional<LoadableStruct>, #Optional.Some!enumelt.1
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, {{%.+}} : $LoadableStruct
  // CHECK:   br [[RETURN:[^ ]+]]([[FINAL_RESULT]] : $Optional<LoadableStruct>)
  // CHECK: [[FAILURE]]:
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<LoadableStruct>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]([[FINAL_RESULT]] : $Optional<LoadableStruct>)
  // CHECK: [[RETURN]]([[RET_VAL:%.+]] : $Optional<LoadableStruct>):
  // CHECK:   return [[RET_VAL]] : $Optional<LoadableStruct>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   [[FAIL_VALUE:%.+]] = enum $Optional<Optional<LoadableStruct>>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]([[FAIL_VALUE]] : $Optional<Optional<LoadableStruct>>)
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(maybe2: Bool) {
    try? self.init(complex: true)
  }
}

extension AddressOnlyStruct {
  init(error: Bool) throws {
    x = C()
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FzT9propagateSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $AddressOnlyStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*AddressOnlyStruct
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FzT5errorSb_S0_
  // CHECK:   [[BOX:%.+]] = alloc_stack $AddressOnlyStruct
  // CHECK:   try_apply [[OTHER_INIT]]([[BOX]]#1, {{%.+}}, %2) : $@convention(thin) (@out AddressOnlyStruct, Bool, @thin AddressOnlyStruct.Type) -> @error ErrorType, normal [[SUCCESS:[^ ]+]], error [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]({{%.+}} : $()):
  // CHECK:   copy_addr [take] [[BOX]]#1 to [[SELF_BOX_VAL]] : $*AddressOnlyStruct
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   copy_addr [[SELF_BOX_VAL]] to [initialization] %0 : $*AddressOnlyStruct
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[VOID:%.+]] = tuple ()
  // CHECK:   return [[VOID]] : $()
  // CHECK: [[FAILURE]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   throw [[ERROR]] : $ErrorType
  // CHECK: {{^}$}}
  init(propagate: Bool) throws {
    try self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT5forceSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $AddressOnlyStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*AddressOnlyStruct
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FzT5errorSb_S0_
  // CHECK:   [[BOX:%.+]] = alloc_stack $AddressOnlyStruct
  // CHECK:   try_apply [[OTHER_INIT]]([[BOX]]#1, {{%.+}}, %2) : $@convention(thin) (@out AddressOnlyStruct, Bool, @thin AddressOnlyStruct.Type) -> @error ErrorType, normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]({{%.+}} : $()):
  // CHECK:   copy_addr [take] [[BOX]]#1 to [[SELF_BOX_VAL]] : $*AddressOnlyStruct
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   copy_addr [[SELF_BOX_VAL]] to [initialization] %0 : $*AddressOnlyStruct
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[VOID:%.+]] = tuple ()
  // CHECK:   return [[VOID]] : $()
  // CHECK: [[FAILURE:[^ ]+]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   builtin "unexpectedError"
  // CHECK: [[CLEANUP]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   br [[FAILURE]]([[ERROR]] : $ErrorType)
  // CHECK: {{^}$}}
  init(force: Bool) {
    try! self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT5maybeSb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $AddressOnlyStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*AddressOnlyStruct
  // CHECK:   [[BOX:%.+]] = alloc_stack $Optional<AddressOnlyStruct>
  // CHECK:   [[BOX_VAL:%.+]] = init_enum_data_addr [[BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FzT5errorSb_S0_
  // CHECK:   try_apply [[OTHER_INIT]]([[BOX_VAL]], {{%.+}}, %2) : $@convention(thin) (@out AddressOnlyStruct, Bool, @thin AddressOnlyStruct.Type) -> @error ErrorType, normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]({{%.+}} : $()):
  // CHECK:   inject_enum_addr [[BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   br [[CHECK:[^ ]+]]
  // CHECK: [[CHECK]]:
  // CHECK:   [[CASE:%.+]] = select_enum_addr [[BOX]]#1 : $*Optional<AddressOnlyStruct>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE_CLEANUP:[^ ]+]]
  // CHECK: [[FAILURE_CLEANUP]]:
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   br [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[VALUE:%.+]] = unchecked_take_enum_data_addr [[BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   copy_addr [take] [[VALUE]] to [[SELF_BOX_VAL]] : $*AddressOnlyStruct
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   [[FINAL_VAL:%.+]] = init_enum_data_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   copy_addr [[SELF_BOX_VAL]] to [initialization] [[FINAL_VAL]] : $*AddressOnlyStruct
  // CHECK:   inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0 : $@box AddressOnlyStruct
  // CHECK:   inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]
  // CHECK: [[RETURN]]:
  // CHECK:   [[VOID:%.+]] = tuple ()
  // CHECK:   return [[VOID]] : $()
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   inject_enum_addr [[BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(maybe: Bool) {
    try? self.init(error: true)
  }

  init?(complex: Bool) throws {
    try self.init(error: true)
  }

  // CHECK-LABEL: sil hidden @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT6maybe2Sb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $AddressOnlyStruct
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*AddressOnlyStruct
  // CHECK:   [[BOX:%.+]] = alloc_stack $Optional<Optional<AddressOnlyStruct>>
  // CHECK:   [[BOX_VAL:%.+]] = init_enum_data_addr [[BOX]]#1 : $*Optional<Optional<AddressOnlyStruct>>, #Optional.Some!enumelt.1
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFV21failable_initializers17AddressOnlyStructCfMS0_FzT7complexSb_GSqS0__
  // CHECK:   try_apply [[OTHER_INIT]]([[BOX_VAL]], {{%.+}}, %2) : $@convention(thin) (@out Optional<AddressOnlyStruct>, Bool, @thin AddressOnlyStruct.Type) -> @error ErrorType, normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]({{%.+}} : $()):
  // CHECK:   inject_enum_addr [[BOX]]#1 : $*Optional<Optional<AddressOnlyStruct>>, #Optional.Some!enumelt.1
  // CHECK:   br [[CHECK:[^ ]+]]
  // CHECK: [[CHECK]]:
  // CHECK:   [[INTERMEDIATE_BOX:%.+]] = alloc_stack $Optional<AddressOnlyStruct>
  // CHECK:   [[CASE:%.+]] = select_enum_addr [[BOX]]#1 : $*Optional<Optional<AddressOnlyStruct>>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[UNWRAP:[^ ]+]], [[NONE:[^ ]+]]
  // CHECK: [[UNWRAP]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_take_enum_data_addr [[BOX]]#1 : $*Optional<Optional<AddressOnlyStruct>>, #Optional.Some!enumelt.1
  // CHECK:   copy_addr [take] [[UNWRAPPED]] to [initialization] [[INTERMEDIATE_BOX]]#1
  // CHECK:   br [[CHECK_MERGED:[^ ]+]]
  // CHECK: [[NONE]]:
  // CHECK:   inject_enum_addr [[INTERMEDIATE_BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:   br [[CHECK_MERGED]]
  // CHECK: [[CHECK_MERGED]]:
  // CHECK:   [[CASE:%.+]] = select_enum_addr [[INTERMEDIATE_BOX]]#1 : $*Optional<AddressOnlyStruct>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE_CLEANUP:[^ ]+]]
  // CHECK: [[FAILURE_CLEANUP]]:
  // CHECK:   dealloc_stack [[INTERMEDIATE_BOX]]#0
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   br [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_take_enum_data_addr [[INTERMEDIATE_BOX]]#1 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   copy_addr [take] [[UNWRAPPED]] to [[SELF_BOX_VAL]]
  // CHECK:   dealloc_stack [[INTERMEDIATE_BOX]]#0
  // CHECK:   dealloc_stack [[BOX]]#0
  // CHECK:   [[RESULT_VAL:%.+]] = init_enum_data_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   copy_addr [[SELF_BOX_VAL]] to [initialization] [[RESULT_VAL]]
  // CHECK:   inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some!enumelt.1
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0 : $@box AddressOnlyStruct
  // CHECK:   inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]
  // CHECK: [[RETURN]]:
  // CHECK:   [[VOID:%.+]] = tuple ()
  // CHECK:   return [[VOID]]
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   inject_enum_addr [[BOX]]#1 : $*Optional<Optional<AddressOnlyStruct>>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(maybe2: Bool) {
    try? self.init(complex: true)
  }
}

class RootClassThrows {
  init() throws {}

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers15RootClassThrowscfMS0_FzT9propagateSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $RootClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*RootClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $RootClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = class_method %6 : $RootClassThrows, #RootClassThrows.init!initializer.1
  // CHECK:   try_apply [[OTHER_INIT]]([[ORIG_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   store [[VALUE]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NEW_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   strong_retain [[NEW_SELF]]
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   return [[NEW_SELF]] : $RootClassThrows
  // CHECK: [[FAILURE]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   throw [[ERROR]] : $ErrorType
  // CHECK: {{^}$}}
  convenience init(propagate: Bool) throws {
    try self.init()
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers15RootClassThrowscfMS0_FT5forceSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $RootClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*RootClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $RootClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = class_method %6 : $RootClassThrows, #RootClassThrows.init!initializer.1
  // CHECK:   try_apply [[OTHER_INIT]]([[ORIG_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   store [[VALUE]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NEW_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   strong_retain [[NEW_SELF]]
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   return [[NEW_SELF]] : $RootClassThrows
  // CHECK: [[FAILURE:[^ ]+]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   builtin "unexpectedError"
  // CHECK: [[CLEANUP]]([[ERROR:%.+]] : $ErrorType):
  // CHECK-NEXT:   br [[FAILURE]]([[ERROR]] : $ErrorType)
  // CHECK: {{^}$}}
  convenience init(force: Bool) {
    try! self.init()
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers15RootClassThrowscfMS0_FT5maybeSb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $RootClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*RootClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $RootClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = class_method %6 : $RootClassThrows, #RootClassThrows.init!initializer.1
  // CHECK:   try_apply [[OTHER_INIT]]([[ORIG_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<RootClassThrows>, #Optional.Some!enumelt.1, [[VALUE]] : $RootClassThrows
  // CHECK:   br [[CHECK:[^ ]+]]([[WRAPPED]] : $Optional<RootClassThrows>)
  // CHECK: [[CHECK]]([[RESULT:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<RootClassThrows>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[RESULT]] : $Optional<RootClassThrows>, #Optional.Some!enumelt.1
  // CHECK:   store [[UNWRAPPED]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[FINAL_RESULT_UNWRAPPED:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   strong_retain [[FINAL_RESULT_UNWRAPPED]]
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<RootClassThrows>, #Optional.Some!enumelt.1, [[FINAL_RESULT_UNWRAPPED]] : $RootClassThrows
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]([[FINAL_RESULT]] : $Optional<RootClassThrows>)
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<RootClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]([[FINAL_RESULT]] : $Optional<RootClassThrows>)
  // CHECK: [[RETURN]]([[RET_VAL:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   return [[RET_VAL]] : $Optional<RootClassThrows>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   [[FAIL_VALUE:%.+]] = enum $Optional<RootClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]([[FAIL_VALUE]] : $Optional<RootClassThrows>)
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  convenience init?(maybe: Bool) {
    try? self.init()
  }

  convenience init?(complex: Bool) throws {
    try self.init()
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers15RootClassThrowscfMS0_FT6maybe2Sb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $RootClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*RootClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $RootClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = class_method %6 : $RootClassThrows, #RootClassThrows.init!initializer.1
  // CHECK:   try_apply [[OTHER_INIT]]({{%.+}}, [[ORIG_SELF]]) : $@convention(method) (Bool, @owned RootClassThrows) -> (@owned Optional<RootClassThrows>, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<Optional<RootClassThrows>>, #Optional.Some!enumelt.1, [[VALUE]]
  // CHECK:   br [[CHECK:[^ ]+]]([[WRAPPED]] : $Optional<Optional<RootClassThrows>>)
  // CHECK: [[CHECK]]([[RESULT:%.+]] : $Optional<Optional<RootClassThrows>>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<Optional<RootClassThrows>>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[UNWRAP:[^ ]+]], [[NONE:[^ ]+]]
  // CHECK: [[UNWRAP]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[RESULT]] : $Optional<Optional<RootClassThrows>>, #Optional.Some!enumelt.1
  // CHECK:   br [[CHECK_MERGED:[^ ]+]]([[UNWRAPPED]] : $Optional<RootClassThrows>)
  // CHECK: [[NONE]]:
  // CHECK:   [[NONE_VAL:%.+]] = enum $Optional<RootClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[CHECK_MERGED]]([[NONE_VAL]] : $Optional<RootClassThrows>)
  // CHECK: [[CHECK_MERGED]]([[RESULT:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<RootClassThrows>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[RESULT]] : $Optional<RootClassThrows>, #Optional.Some!enumelt.1
  // CHECK:   store [[UNWRAPPED]] to [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   [[FINAL_RESULT_UNWRAPPED:%.+]] = load [[SELF_BOX_VAL]] : $*RootClassThrows
  // CHECK:   strong_retain [[FINAL_RESULT_UNWRAPPED]]
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<RootClassThrows>, #Optional.Some!enumelt.1, [[FINAL_RESULT_UNWRAPPED]] : $RootClassThrows
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]([[FINAL_RESULT]] : $Optional<RootClassThrows>)
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<RootClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]([[FINAL_RESULT]] : $Optional<RootClassThrows>)
  // CHECK: [[RETURN]]([[RET_VAL:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   return [[RET_VAL]] : $Optional<RootClassThrows>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   [[FAIL_VALUE:%.+]] = enum $Optional<Optional<RootClassThrows>>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]([[FAIL_VALUE]] : $Optional<Optional<RootClassThrows>>)
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  convenience init?(maybe2: Bool) {
    try? self.init(complex: true)
  }
}

class SubClassThrows : RootClassThrows {
  // CHECK-LABEL: sil hidden @_TFC21failable_initializers14SubClassThrowscfMS0_FzT9propagateSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $SubClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [derivedself] [[SELF_BOX]]#1 : $*SubClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $SubClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[BASE_SELF:%.+]] = upcast [[ORIG_SELF]] : $SubClassThrows to $RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFC21failable_initializers15RootClassThrowscfMS0_FzT_S0_
  // CHECK:   try_apply [[OTHER_INIT]]([[BASE_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   [[DOWNCAST_VALUE:%.+]] = unchecked_ref_cast [[VALUE]] : $RootClassThrows to $SubClassThrows // user: %14
  // CHECK:   store [[DOWNCAST_VALUE]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[NEW_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   strong_retain [[NEW_SELF]]
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   return [[NEW_SELF]] : $SubClassThrows
  // CHECK: [[FAILURE]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   throw [[ERROR]] : $ErrorType
  // CHECK: {{^}$}}
  init(propagate: Bool) throws {
    try super.init()
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers14SubClassThrowscfMS0_FT5forceSb_S0_
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $SubClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [derivedself] [[SELF_BOX]]#1 : $*SubClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $SubClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[BASE_SELF:%.+]] = upcast [[ORIG_SELF]] : $SubClassThrows to $RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFC21failable_initializers15RootClassThrowscfMS0_FzT_S0_
  // CHECK:   try_apply [[OTHER_INIT]]([[BASE_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   [[DOWNCAST_VALUE:%.+]] = unchecked_ref_cast [[VALUE]] : $RootClassThrows to $SubClassThrows // user: %14
  // CHECK:   store [[DOWNCAST_VALUE]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[NEW_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   strong_retain [[NEW_SELF]]
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   return [[NEW_SELF]] : $SubClassThrows
  // CHECK: [[FAILURE:[^ ]+]]([[ERROR:%.+]] : $ErrorType):
  // CHECK:   builtin "unexpectedError"
  // CHECK: [[CLEANUP]]([[ERROR:%.+]] : $ErrorType):
  // CHECK-NEXT:   br [[FAILURE]]([[ERROR]] : $ErrorType)
  // CHECK: {{^}$}}
  init(force: Bool) {
    try! super.init()
  }

  // CHECK-LABEL: sil hidden @_TFC21failable_initializers14SubClassThrowscfMS0_FT5maybeSb_GSqS0__
  // CHECK:   [[SELF_BOX:%.+]] = alloc_box $SubClassThrows
  // CHECK:   [[SELF_BOX_VAL:%.+]] = mark_uninitialized [derivedself] [[SELF_BOX]]#1 : $*SubClassThrows
  // CHECK:   [[ORIG_SELF:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[NULL:%.+]] = null_class $SubClassThrows
  // CHECK:   store [[NULL]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[BASE_SELF:%.+]] = upcast [[ORIG_SELF]] : $SubClassThrows to $RootClassThrows
  // CHECK:   [[OTHER_INIT:%.+]] = function_ref @_TFC21failable_initializers15RootClassThrowscfMS0_FzT_S0_
  // CHECK:   try_apply [[OTHER_INIT]]([[BASE_SELF]]) : $@convention(method) (@owned RootClassThrows) -> (@owned RootClassThrows, @error ErrorType), normal [[SUCCESS:[^ ]+]], error [[CLEANUP:[^ ]+]]
  // CHECK: [[SUCCESS]]([[VALUE:%.+]] : $RootClassThrows):
  // CHECK:   [[WRAPPED:%.+]] = enum $Optional<RootClassThrows>, #Optional.Some!enumelt.1, [[VALUE]] : $RootClassThrows
  // CHECK:   br [[CHECK:[^ ]+]]([[WRAPPED]] : $Optional<RootClassThrows>)
  // CHECK: [[CHECK]]([[RESULT:%.+]] : $Optional<RootClassThrows>):
  // CHECK:   [[CASE:%.+]] = select_enum [[RESULT]] : $Optional<RootClassThrows>, case #Optional.Some!enumelt.1: {{%.+}}, default {{%.+}}
  // CHECK:   cond_br [[CASE]], [[SUCCESS:[^ ]+]], [[FAILURE:[^ ]+]]
  // CHECK: [[SUCCESS]]:
  // CHECK:   [[UNWRAPPED:%.+]] = unchecked_enum_data [[RESULT]] : $Optional<RootClassThrows>, #Optional.Some!enumelt.1
  // CHECK:   [[DOWNCAST:%.+]] = unchecked_ref_cast [[UNWRAPPED]] : $RootClassThrows to $SubClassThrows
  // CHECK:   store [[DOWNCAST]] to [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   [[FINAL_RESULT_UNWRAPPED:%.+]] = load [[SELF_BOX_VAL]] : $*SubClassThrows
  // CHECK:   strong_retain [[FINAL_RESULT_UNWRAPPED]]
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<SubClassThrows>, #Optional.Some!enumelt.1, [[FINAL_RESULT_UNWRAPPED]] : $SubClassThrows
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   br [[RETURN:[^ ]+]]([[FINAL_RESULT]] : $Optional<SubClassThrows>)
  // CHECK: [[FAILURE]]:
  // CHECK:   strong_release [[SELF_BOX]]#0
  // CHECK:   [[FINAL_RESULT:%.+]] = enum $Optional<SubClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[RETURN]]([[FINAL_RESULT]] : $Optional<SubClassThrows>)
  // CHECK: [[RETURN]]([[RET_VAL:%.+]] : $Optional<SubClassThrows>):
  // CHECK:   return [[RET_VAL]] : $Optional<SubClassThrows>
  // CHECK: [[ERROR:[^ ]+]]({{%.+}} : $ErrorType):
  // CHECK:   [[FAIL_VALUE:%.+]] = enum $Optional<RootClassThrows>, #Optional.None!enumelt
  // CHECK:   br [[CHECK]]([[FAIL_VALUE]] : $Optional<RootClassThrows>)
  // CHECK: [[CLEANUP]]({{%.+}} : $ErrorType):
  // CHECK-NEXT:   br [[ERROR]]({{%.+}} : $ErrorType)
  // CHECK: {{^}$}}
  init?(maybe: Bool) {
    try? super.init()
  }
}
