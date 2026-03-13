// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s
// RUN:%target-swift-frontend -c %s -enable-experimental-feature BorrowAndMutateAccessors -Xllvm -sil-print-after=SILGenCleanup 2>&1 | %FileCheck %s --check-prefixes=CHECK-SIL
// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK-EVO

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public protocol P {
  var id: NonTrivial { borrow mutate }
}

public class Klass {
  public init() {}
}

public struct NonTrivial {
  public var k = Klass()
}

// Protocol requirements witnessed via borrow/mutate accessors
public struct S1 : P {
  var _id: NonTrivial
  var _k: Klass

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S1) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*S1):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S1V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S1) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed S1) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S1) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*S1):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S1V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S1) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed S1) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S1) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*S1):
// CHECK-EVO:   [[REG1:%.*]] = function_ref @$s24borrow_accessor_protocol2S1V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// Protocol requirements witnessed via stored property
public struct S2 : P {
  public var id: NonTrivial
}

public struct S3 : P {
  var _id: NonTrivial
}

extension S3 {
  public var id: NonTrivial {
    borrow  {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S2) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*S2):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S2V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S2) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed S2) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S2) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*S2):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S2V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S2) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed S2) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S2) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*S2):
// CHECK-EVO:   [[REG1:%.*]] = function_ref @$s24borrow_accessor_protocol2S2V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S2) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed S2) -> @guaranteed_address NonTrivial
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

@frozen
public struct S4 : P {
  var _id: NonTrivial

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S4) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*S4):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S4V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S4) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed S4) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S4) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*S4):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S4V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S4) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed S4) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-EVO: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S4) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*S4):
// CHECK-EVO:   [[REG1:%.*]] = function_ref @$s24borrow_accessor_protocol2S4V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S4) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed S4) -> @guaranteed_address NonTrivial
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

@frozen
public struct S5 : P {
  public var id: NonTrivial
}

public protocol Q {
  var id: FrozenNonTrivial { borrow mutate }
}

@frozen
public struct FrozenNonTrivial {
  public var k = Klass()
}

public struct S6 : Q {
  var _id: FrozenNonTrivial

  public var id: FrozenNonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

public protocol R {
  var id: NonTrivial { borrow }
}

public class K1 : R {
  public let id: NonTrivial

  init(_ id: NonTrivial) {
    self.id = id
  }
}

public class K2 : R {
  let _id = NonTrivial()

  public var id: NonTrivial {
    borrow {
      return _id
    }
  }
}

public class K3 : R {
  let _id = NonTrivial()
}

extension K3 {
  public var id: NonTrivial {
    borrow  {
      return _id
    }
  }
}

public protocol S {
  var id: FrozenNonTrivial { borrow }
}

public class K4 : S {
  let _id = FrozenNonTrivial()

  public var id: FrozenNonTrivial {
    borrow {
      return _id
    }
  }
}

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S5) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*S5):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S5) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed S5) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S5) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*S5):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvb : $@convention(method) (@guaranteed S5) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed S5) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-EVO: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S5) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*S5):
// CHECK-EVO:   [[REG1:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2S6VAA1QA2aDP2idAA16FrozenNonTrivialVvbTW : $@convention(witness_method: Q) (@in_guaranteed S6) -> @guaranteed FrozenNonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*S6):
// CHECK-EVO:   [[REG1:%.*]] = function_ref @$s24borrow_accessor_protocol2S6V2idAA16FrozenNonTrivialVvb : $@convention(method) (@in_guaranteed S6) -> @guaranteed FrozenNonTrivial
// CHECK-EVO:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed S6) -> @guaranteed FrozenNonTrivial
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K1CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K1) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*K1):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil shared [serialized] [ossa] @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $K1):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K1.id
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership [[REG3]]
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil [ossa] @$s24borrow_accessor_protocol2K2C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $K2):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K2._id
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership [[REG3]]
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K2CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K2) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*K2):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = class_method [[REG1]], #K2.id!borrow : (K2) -> () -> NonTrivial, $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K3CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K3) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*K3):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil [ossa] @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $K3):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K3._id
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership [[REG3]]
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil [ossa] @$s24borrow_accessor_protocol2K4C2idAA16FrozenNonTrivialVvb : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $K4):
// CHECK:   debug_value [[REG0]], let, name "self", argno 1
// CHECK:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K4._id
// CHECK:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK:   [[REG4:%.*]] = unchecked_ownership [[REG3]]
// CHECK:   end_borrow [[REG3]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K4CAA1SA2aDP2idAA16FrozenNonTrivialVvbTW : $@convention(witness_method: S) (@in_guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK: bb0([[REG0:%.*]] : $*K4):
// CHECK:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK:   [[REG2:%.*]] = class_method [[REG1]], #K4.id!borrow : (K4) -> () -> FrozenNonTrivial, $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK:   end_borrow [[REG1]]
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K1CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K1) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*K1):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-SIL: sil shared [serialized] [ossa] @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : @guaranteed $K1):
// CHECK-SIL:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-SIL:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K1.id
// CHECK-SIL:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG3]])
// CHECK-SIL: }

// CHECK-SIL: sil [ossa] @$s24borrow_accessor_protocol2K2C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : @guaranteed $K2):
// CHECK-SIL:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-SIL:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K2._id
// CHECK-SIL:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG3]])
// CHECK-SIL: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K2CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K2) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*K2):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = class_method [[REG1]], #K2.id!borrow : (K2) -> () -> NonTrivial, $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K2) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K3CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K3) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*K3):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-SIL: sil [ossa] @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed NonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : @guaranteed $K3):
// CHECK-SIL:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-SIL:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K3._id
// CHECK-SIL:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG3]])
// CHECK-SIL: }

// CHECK-SIL: sil [ossa] @$s24borrow_accessor_protocol2K4C2idAA16FrozenNonTrivialVvb : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : @guaranteed $K4):
// CHECK-SIL:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-SIL:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K4._id
// CHECK-SIL:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG3]])
// CHECK-SIL: }

// CHECK-SIL: sil shared [transparent] [serialized] [thunk] [ossa] @$s24borrow_accessor_protocol2K4CAA1SA2aDP2idAA16FrozenNonTrivialVvbTW : $@convention(witness_method: S) (@in_guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK-SIL: bb0([[REG0:%.*]] : $*K4):
// CHECK-SIL:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-SIL:   [[REG2:%.*]] = class_method [[REG1]], #K4.id!borrow : (K4) -> () -> FrozenNonTrivial, $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK-SIL:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK-SIL:   return_borrow [[REG3]] from_scopes ([[REG1]])
// CHECK-SIL: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2K1CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K1) -> @guaranteed_address NonTrivial {

// CHECK-EVO: bb0([[REG0:%.*]] : $*K1):
// CHECK-EVO:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-EVO:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K1) -> @guaranteed_address NonTrivial
// CHECK-EVO:   end_borrow [[REG1]]
// CHECK-EVO:   return [[REG3]]
// CHECK-EVO: }

// CHECK-EVO: sil shared [ossa] @$s24borrow_accessor_protocol2K1C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K1) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : @guaranteed $K1):
// CHECK-EVO:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-EVO:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K1.id
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// CHECK-EVO: sil [ossa] @$s24borrow_accessor_protocol2K2C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K2) -> @guaranteed_address NonTrivial {

// CHECK-EVO: bb0([[REG0:%.*]] : @guaranteed $K2):
// CHECK-EVO:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-EVO:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K2._id
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2K2CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K2) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*K2):
// CHECK-EVO:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-EVO:   [[REG2:%.*]] = class_method [[REG1]], #K2.id!borrow : (K2) -> () -> NonTrivial, $@convention(method) (@guaranteed K2) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K2) -> @guaranteed_address NonTrivial
// CHECK-EVO:   end_borrow [[REG1]]
// CHECK-EVO:   return [[REG3]]
// CHECK-EVO: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2K3CAA1RA2aDP2idAA10NonTrivialVvbTW : $@convention(witness_method: R) (@in_guaranteed K3) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*K3):
// CHECK-EVO:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-EVO:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed_address NonTrivial
// CHECK-EVO:   [[REG3:%.*]] = apply [[REG2]]([[REG1]]) : $@convention(method) (@guaranteed K3) -> @guaranteed_address NonTrivial
// CHECK-EVO:   end_borrow [[REG1]]
// CHECK-EVO:   return [[REG3]]
// CHECK-EVO: }

// CHECK-EVO: sil [ossa] @$s24borrow_accessor_protocol2K3C2idAA10NonTrivialVvb : $@convention(method) (@guaranteed K3) -> @guaranteed_address NonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : @guaranteed $K3):
// CHECK-EVO:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-EVO:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K3._id
// CHECK-EVO:   return [[REG2]]
// CHECK-EVO: }

// CHECK-EVO: sil [ossa] @$s24borrow_accessor_protocol2K4C2idAA16FrozenNonTrivialVvb : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : @guaranteed $K4):
// CHECK-EVO:   debug_value [[REG0]], let, name "self", argno 1
// CHECK-EVO:   [[REG2:%.*]] = ref_element_addr [[REG0]], #K4._id
// CHECK-EVO:   [[REG3:%.*]] = load_borrow [[REG2]]
// CHECK-EVO:   [[REG4:%.*]] = unchecked_ownership [[REG3]]
// CHECK-EVO:   end_borrow [[REG3]]
// CHECK-EVO:   return [[REG4]]
// CHECK-EVO: }

// CHECK-EVO: sil private [transparent] [thunk] [ossa] @$s24borrow_accessor_protocol2K4CAA1SA2aDP2idAA16FrozenNonTrivialVvbTW : $@convention(witness_method: S) (@in_guaranteed K4) -> @guaranteed FrozenNonTrivial {
// CHECK-EVO: bb0([[REG0:%.*]] : $*K4):
// CHECK-EVO:   [[REG1:%.*]] = load_borrow [[REG0]]
// CHECK-EVO:   [[REG2:%.*]] = class_method [[REG1]], #K4.id!borrow : (K4) -> () -> FrozenNonTrivial, $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK-EVO:   [[REG3:%.*]] = unchecked_ownership [[REG1]]
// CHECK-EVO:   [[REG4:%.*]] = apply [[REG2]]([[REG3]]) : $@convention(method) (@guaranteed K4) -> @guaranteed FrozenNonTrivial
// CHECK-EVO:   end_borrow [[REG1]]
// CHECK-EVO:   return [[REG4]]
// CHECK-EVO: }

// CHECK: sil_witness_table [serialized] S1: P module borrow_accessor_protocol {
// CHECK:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S1
// CHECK:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S1
// CHECK: }

// CHECK: sil_witness_table [serialized] S2: P module borrow_accessor_protocol {
// CHECK:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S2
// CHECK:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S2
// CHECK: }

// CHECK: sil_witness_table [serialized] S3: P module borrow_accessor_protocol {
// CHECK:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S3VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S3
// CHECK:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S3VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S3
// CHECK: }

// CHECK: sil_witness_table [serialized] S4: P module borrow_accessor_protocol {
// CHECK:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S4
// CHECK:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S4
// CHECK: }

// CHECK: sil_witness_table [serialized] S5: P module borrow_accessor_protocol {
// CHECK:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S5
// CHECK:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S5
// CHECK: }

// CHECK: sil_witness_table [serialized] K1: R module borrow_accessor_protocol {
// CHECK:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K1CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK: }

// CHECK: sil_witness_table [serialized] K2: R module borrow_accessor_protocol {
// CHECK:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K2CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK: }

// CHECK: sil_witness_table [serialized] K3: R module borrow_accessor_protocol {
// CHECK:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K3CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK: }

// CHECK: sil_witness_table [serialized] K4: S module borrow_accessor_protocol {
// CHECK:   method #S.id!borrow: <Self where Self : S> (Self) -> () -> FrozenNonTrivial : @$s24borrow_accessor_protocol2K4CAA1SA2aDP2idAA16FrozenNonTrivialVvbTW
// CHECK: }

// CHECK-EVO: sil_witness_table S1: P module borrow_accessor_protocol {
// CHECK-EVO:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S1
// CHECK-EVO:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S1VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S1
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table S2: P module borrow_accessor_protocol {
// CHECK-EVO:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S2
// CHECK-EVO:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S2VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S2
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table S3: P module borrow_accessor_protocol {
// CHECK-EVO:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S3VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S3
// CHECK-EVO:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S3VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S3
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table [serialized] S4: P module borrow_accessor_protocol {
// CHECK-EVO:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S4
// CHECK-EVO:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S4VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S4
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table [serialized] S5: P module borrow_accessor_protocol {
// CHECK-EVO:   method #P.id!borrow: <Self where Self : P> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvbTW	// protocol witness for P.id.borrow in conformance S5
// CHECK-EVO:   method #P.id!mutate: <Self where Self : P> (inout Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2S5VAA1PA2aDP2idAA10NonTrivialVvzTW	// protocol witness for P.id.mutate in conformance S5
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table K1: R module borrow_accessor_protocol {
// CHECK-EVO:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K1CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table K2: R module borrow_accessor_protocol {
// CHECK-EVO:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K2CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table K3: R module borrow_accessor_protocol {
// CHECK-EVO:   method #R.id!borrow: <Self where Self : R> (Self) -> () -> NonTrivial : @$s24borrow_accessor_protocol2K3CAA1RA2aDP2idAA10NonTrivialVvbTW
// CHECK-EVO: }

// CHECK-EVO: sil_witness_table K4: S module borrow_accessor_protocol {
// CHECK-EVO:   method #S.id!borrow: <Self where Self : S> (Self) -> () -> FrozenNonTrivial : @$s24borrow_accessor_protocol2K4CAA1SA2aDP2idAA16FrozenNonTrivialVvbTW
// CHECK-EVO: }

