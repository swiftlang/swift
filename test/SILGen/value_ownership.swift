// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// REQUIRES: plus_zero_runtime

protocol OwnershipProto {
  __consuming func elided(_ default: String, _ shared: __shared String, _ owned: __owned String)

  __consuming func explicit(_ default: String, _ shared: __shared String, _ owned: __owned String)
}

struct Witness: OwnershipProto {
  func elided(_ default: String, _ shared: String, _ owned: String) { }

  __consuming func explicit(_ default: String, _ toShared: __shared String, _ toOwned: __owned String) { }
}

// Check the conventions of the witnesses

// CHECK-LABEL: sil hidden @$S15value_ownership7WitnessV6elidedyySS_S2StF : $@convention(method) (@guaranteed String, @guaranteed String, @guaranteed String, Witness) -> () {
// CHECK:       } // end sil function '$S15value_ownership7WitnessV6elidedyySS_S2StF'

// CHECK-LABEL: sil hidden @$S15value_ownership7WitnessV8explicityySS_SShSSntF : $@convention(method) (@guaranteed String, @guaranteed String, @owned String, Witness) -> () {
// CHECK:       } // end sil function '$S15value_ownership7WitnessV8explicityySS_SShSSntF'

// Check the elided witness' thunk has the right conventions and borrows where necessary

// CHECK-LABEL: @$S15value_ownership7WitnessVAA14OwnershipProtoA2aDP6elidedyySS_SShSSntFTW : $@convention(witness_method: OwnershipProto) (@guaranteed String, @guaranteed String, @owned String, @in_guaranteed Witness) -> ()
// CHECK:       bb0([[DEFAULT2DEFAULT:%.*]] : @guaranteed $String, [[SHARED2DEFAULT:%.*]] : @guaranteed $String, [[OWNED2DEFAULT:%.*]] : @owned $String, [[WITNESS_VALUE:%.*]] : @trivial $*Witness):
// CHECK:         [[LOAD_WITNESS:%.*]] = load [trivial] [[WITNESS_VALUE]] : $*Witness
// CHECK:         [[WITNESS_FUNC:%.*]] = function_ref @$S15value_ownership7WitnessV6elidedyySS_S2StF
// CHECK:         [[BORROWOWNED2DEFAULT:%.*]] = begin_borrow [[OWNED2DEFAULT]] : $String
// CHECK:         apply [[WITNESS_FUNC]]([[DEFAULT2DEFAULT]], [[SHARED2DEFAULT]], [[BORROWOWNED2DEFAULT]], [[LOAD_WITNESS]])
// CHECK:         end_borrow [[BORROWOWNED2DEFAULT]] from [[OWNED2DEFAULT]] : $String, $String
// CHECK:       } // end sil function '$S15value_ownership7WitnessVAA14OwnershipProtoA2aDP6elidedyySS_SShSSntFTW'

// Check that the explicit witness' thunk doesn't copy or borrow

// CHECK-LABEL: @$S15value_ownership7WitnessVAA14OwnershipProtoA2aDP8explicityySS_SShSSntFTW : $@convention(witness_method: OwnershipProto) (@guaranteed String, @guaranteed String, @owned String, @in_guaranteed Witness) -> () {
// CHECK:       bb0([[ARG0:%.*]] : @guaranteed $String, [[ARG1:%.*]] : @guaranteed $String, [[ARG2:%.*]] : @owned $String, [[WITNESS_VALUE:%.*]] : @trivial $*Witness):
// CHECK-NEXT:    [[LOAD_WITNESS:%.*]] = load [trivial] [[WITNESS_VALUE]] : $*Witness
// CHECK-NEXT:    // function_ref Witness.explicit(_:_:_:)
// CHECK-NEXT:    [[WITNESS_FUNC:%.*]] = function_ref @$S15value_ownership7WitnessV8explicityySS_SShSSntF
// CHECK-NEXT:    apply [[WITNESS_FUNC]]([[ARG0]], [[ARG1]], [[ARG2]], [[LOAD_WITNESS]])
// CHECK:       } // end sil function '$S15value_ownership7WitnessVAA14OwnershipProtoA2aDP8explicityySS_SShSSntFTW'
