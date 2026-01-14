// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s
// RUN:%target-swift-frontend -c %s -enable-experimental-feature BorrowAndMutateAccessors -Xllvm -sil-print-after=SILGenCleanup 2>&1 | %FileCheck %s --check-prefixes=CHECK-SIL
// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors -enable-library-evolution | %FileCheck %s --check-prefixes=CHECK-EVO

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public protocol P {
  var id: NonTrivial { borrow mutate }
}

public class Klass {}

public struct NonTrivial {
  public var k: Klass
}

// Protocol requirements witnessed via borrow/mutate accessors
public struct S1 : P {
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
