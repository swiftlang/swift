// RUN: %swift -emit-silgen %s | FileCheck %s

protocol P {}
class C: P {}

struct LoadableStruct {
  var x: C

  // CHECK-LABEL: sil @_TFV21failable_initializers14LoadableStructCfMS0_FT3optSb_GSqS0__
  init?(opt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // TODO: failure
  // CHECK:       bb{{.*}}:
  // CHECK:         unreachable
    if opt {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[SELF:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_OPT:%.*]] = enum $Optional<LoadableStruct>, #Optional.Some!enumelt.1, [[SELF]]
  // CHECK:         return [[SELF_OPT]]
  }

  // CHECK-LABEL: sil @_TFV21failable_initializers14LoadableStructCfMS0_FT3iuoSb_GSQS0__
  init!(iuo: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $LoadableStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // TODO: failure
  // CHECK:       bb{{.*}}:
  // CHECK:         unreachable
    if iuo {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[SELF:%.*]] = load [[SELF_MARKED]]
  // CHECK:         [[SELF_OPT:%.*]] = enum $ImplicitlyUnwrappedOptional<LoadableStruct>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[SELF]]
  // CHECK:         return [[SELF_OPT]]
  }

  /*
  init?(delegatesOptOpt: Bool) {
    self.init(opt: true)
  }

  init!(delegatesIUOIUO: Bool) {
    self.init(iuo: true)
  }

  init?(delegatesOptIUO: Bool) {
    self.init(iuo: true)
  }

  init!(delegatesIUOOpt: Bool) {
    self.init(opt: true)
  }

  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
   */
}

struct AddressOnlyStruct {
  var x: P

  // CHECK-LABEL: sil @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3optSb_GSqS0__
  init?(opt: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // TODO: failure
  // CHECK:       bb{{.*}}:
  // CHECK:         unreachable
    if opt {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[DEST_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some
  // CHECK:         copy_addr [[SELF_MARKED]] to [initialization] [[DEST_PAYLOAD]]
  // CHECK:         inject_enum_addr %0 : $*Optional<AddressOnlyStruct>, #Optional.Some
  }

  // CHECK-LABEL: sil @_TFV21failable_initializers17AddressOnlyStructCfMS0_FT3iuoSb_GSQS0__
  init!(iuo: Bool) {
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $AddressOnlyStruct
  // CHECK:         [[SELF_MARKED:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]#1
    x = C()
  // TODO: failure
  // CHECK:       bb{{.*}}:
  // CHECK:         unreachable
    if iuo {
      return nil
    }

  // CHECK:       bb{{.*}}:
  // CHECK:         [[DEST_PAYLOAD:%.*]] = init_enum_data_addr %0 : $*ImplicitlyUnwrappedOptional<AddressOnlyStruct>, #ImplicitlyUnwrappedOptional.Some
  // CHECK:         copy_addr [[SELF_MARKED]] to [initialization] [[DEST_PAYLOAD]]
  // CHECK:         inject_enum_addr %0 : $*ImplicitlyUnwrappedOptional<AddressOnlyStruct>, #ImplicitlyUnwrappedOptional.Some
  }

  /*
  init?(delegatesOptOpt: Bool) {
    self.init(opt: true)
  }

  init!(delegatesIUOIUO: Bool) {
    self.init(iuo: true)
  }

  init?(delegatesOptIUO: Bool) {
    self.init(iuo: true)
  }

  init!(delegatesIUOOpt: Bool) {
    self.init(opt: true)
  }

  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
   */
}

enum LoadableEnum {
  case A(C)

  init?(opt: Bool) {
    self = A(C())

    if opt {
      return nil
    }
  }

  init!(iuo: Bool) {
    self = A(C())

    if iuo {
      return nil
    }
  }

  /*
  init?(delegatesOptOpt: Bool) {
    self.init(opt: true)
  }

  init!(delegatesIUOIUO: Bool) {
    self.init(iuo: true)
  }

  init?(delegatesOptIUO: Bool) {
    self.init(iuo: true)
  }

  init!(delegatesIUOOpt: Bool) {
    self.init(opt: true)
  }

  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
   */
}

enum AddressOnlyEnum {
  case A(P)

  init?(opt: Bool) {
    self = A(C())

    if opt {
      return nil
    }
  }

  init!(iuo: Bool) {
    self = A(C())

    if iuo {
      return nil
    }
  }

  /*
  init?(delegatesOptOpt: Bool) {
    self.init(opt: true)
  }

  init!(delegatesIUOIUO: Bool) {
    self.init(iuo: true)
  }

  init?(delegatesOptIUO: Bool) {
    self.init(iuo: true)
  }

  init!(delegatesIUOOpt: Bool) {
    self.init(opt: true)
  }

  init(delegatesNormIUO: Bool) {
    self.init(iuo: true)
  }
   */
}


