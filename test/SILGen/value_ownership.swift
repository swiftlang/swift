// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

protocol OwnershipProto {
  __consuming func elided(_ default: String, _ shared: __shared String, _ owned: __owned String)

  __consuming func explicit(_ default: String, _ shared: __shared String, _ owned: __owned String)

  var elidedPropertyGet: String { __consuming get }
  var explicitPropertyGet: String { __consuming get }
}

struct Witness: OwnershipProto {
  var x: String

  func elided(_ default: String, _ shared: String, _ owned: String) { }

  __consuming func explicit(_ default: String, _ toShared: __shared String, _ toOwned: __owned String) { }

  var elidedPropertyGet: String { return "" }
  var explicitPropertyGet: String { __consuming get { return "" } }
}

// Check the conventions of the witnesses

// CHECK-LABEL: sil hidden @$s15value_ownership7WitnessV6elidedyySS_S2StF : $@convention(method) (@guaranteed String, @guaranteed String, @guaranteed String, @guaranteed Witness) -> () {
// CHECK-LABEL: sil hidden @$s15value_ownership7WitnessV8explicityySS_SShSSntF : $@convention(method) (@guaranteed String, @guaranteed String, @owned String, @owned Witness) -> () {
// CHECK-LABEL: sil hidden @$s15value_ownership7WitnessV17elidedPropertyGetSSvg : $@convention(method) (@guaranteed Witness) -> @owned String
// CHECK-LABEL: sil hidden @$s15value_ownership7WitnessV19explicitPropertyGetSSvg : $@convention(method) (@owned Witness) -> @owned String

// Check the elided witness' thunk has the right conventions and borrows where necessary

// CHECK-LABEL: @$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP6elidedyySS_SShSSntFTW : $@convention(witness_method: OwnershipProto) (@guaranteed String, @guaranteed String, @owned String, @in Witness) -> ()
// CHECK:       bb0([[DEFAULT2DEFAULT:%.*]] : @guaranteed $String, [[SHARED2DEFAULT:%.*]] : @guaranteed $String, [[OWNED2DEFAULT:%.*]] : @owned $String, [[WITNESS_VALUE:%.*]] : $*Witness):
// CHECK:         [[LOAD_WITNESS:%.*]] = load [take] [[WITNESS_VALUE]] : $*Witness
// CHECK:         [[BORROW_WITNESS:%.*]] = begin_borrow [[LOAD_WITNESS]]
// CHECK:         [[WITNESS_FUNC:%.*]] = function_ref @$s15value_ownership7WitnessV6elidedyySS_S2StF
// CHECK:         [[BORROWOWNED2DEFAULT:%.*]] = begin_borrow [[OWNED2DEFAULT]] : $String
// CHECK:         apply [[WITNESS_FUNC]]([[DEFAULT2DEFAULT]], [[SHARED2DEFAULT]], [[BORROWOWNED2DEFAULT]], [[BORROW_WITNESS]])
// CHECK:         end_borrow [[BORROWOWNED2DEFAULT]] : $String
// CHECK:         end_borrow [[BORROW_WITNESS]]
// CHECK:         destroy_value [[LOAD_WITNESS]]
// CHECK:       } // end sil function '$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP6elidedyySS_SShSSntFTW'

// Check that the explicit witness' thunk doesn't copy or borrow

// CHECK-LABEL: @$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP8explicityySS_SShSSntFTW : $@convention(witness_method: OwnershipProto) (@guaranteed String, @guaranteed String, @owned String, @in Witness) -> () {
// CHECK:       bb0([[ARG0:%.*]] : @guaranteed $String, [[ARG1:%.*]] : @guaranteed $String, [[ARG2:%.*]] : @owned $String, [[WITNESS_VALUE:%.*]] : $*Witness):
// CHECK-NEXT:    [[LOAD_WITNESS:%.*]] = load [take] [[WITNESS_VALUE]] : $*Witness
// CHECK-NEXT:    // function_ref Witness.explicit(_:_:_:)
// CHECK-NEXT:    [[WITNESS_FUNC:%.*]] = function_ref @$s15value_ownership7WitnessV8explicityySS_SShSSntF
// CHECK-NEXT:    apply [[WITNESS_FUNC]]([[ARG0]], [[ARG1]], [[ARG2]], [[LOAD_WITNESS]])
// CHECK:       } // end sil function '$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP8explicityySS_SShSSntFTW'

// Check the signature of the property accessor witness thunks.
// If a protocol asks for a __consuming get it should get a +1-in +1-out
// accessor entry point in its witness table.

// CHECK-LABEL: sil private [transparent] [thunk] @$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP17elidedPropertyGetSSvgTW : $@convention(witness_method: OwnershipProto) (@in Witness) -> @owned String
// CHECK-LABEL: sil private [transparent] [thunk] @$s15value_ownership7WitnessVAA14OwnershipProtoA2aDP19explicitPropertyGetSSvgTW : $@convention(witness_method: OwnershipProto) (@in Witness) -> @owned String
