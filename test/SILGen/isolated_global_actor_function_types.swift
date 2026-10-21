// RUN: %target-swift-emit-silgen -enable-experimental-feature AllowIsolatedGlobalActorAttribute -target %target-swift-5.1-abi-triple -module-name main %s | %FileCheck %s --check-prefixes=CHECK
// RUN: %target-swift-emit-silgen -enable-experimental-feature AllowIsolatedGlobalActorAttribute -target %target-swift-5.1-abi-triple -swift-version 6 -module-name main %s | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: concurrency
// REQUIRES: swift_feature_AllowIsolatedGlobalActorAttribute

// The generic global actor's executor is loaded through the GlobalActor.shared
// protocol requirement. Global actor isolated function types are implicitly
// '@Sendable' in the Swift 6 language mode, which changes the mangling ('Yb')

// CHECK-LABEL: sil hidden [ossa] @$s4main15callsIsolatedToyyxm_yy{{(Yb)?}}xYcctYas11GlobalActorRzlF : $@convention(thin) @async <G where G : GlobalActor> (@thick G.Type, @guaranteed {{(@Sendable )?}}@callee_guaranteed () -> ()) -> ()
// CHECK:   [[META:%.*]] = metatype $@thick G.Type
// CHECK:   [[SHARED:%.*]] = witness_method $G, #GlobalActor.shared!getter
// CHECK:   [[ACTOR:%.*]] = apply [[SHARED]]<G>([[META]])
// CHECK:   [[BORROW:%.*]] = begin_borrow [[ACTOR]]
// CHECK:   hop_to_executor [[BORROW]]
// CHECK:   apply
// CHECK: } // end sil function '$s4main15callsIsolatedToyyxm_yy{{(Yb)?}}xYcctYas11GlobalActorRzlF'
func callsIsolatedTo<G: GlobalActor>(
  _: G.Type, _ fn: @escaping @isolated(G) () -> Void
) async {
  await fn()
}

// '#isolation' in a closure isolated to a generic global actor loads 'G.shared'.
// Like for '@MainActor', a synchronous closure doesn't check its executor on
// entry, in either language mode

// CHECK-LABEL: // closure #1 in closureIsolatedTo<A>(_:)
// CHECK-NEXT: // Isolation: global_actor. type: G
// CHECK-NEXT: sil private [ossa] @$s4main17closureIsolatedToyyxms11GlobalActorRzlFyy{{(Yb)?}}xYccfU_ : $@convention(thin) {{(@Sendable )?}}<G where G : GlobalActor> () -> ()
// CHECK:   [[META:%.*]] = metatype $@thick G.Type
// CHECK:   [[SHARED:%.*]] = witness_method $G, #GlobalActor.shared!getter
// CHECK:   [[ACTOR:%.*]] = apply [[SHARED]]<G>([[META]])
// CHECK:   [[EXISTENTIAL:%.*]] = init_existential_ref [[ACTOR]] : $G.ActorType : $G.ActorType, $any Actor
// CHECK:   enum $Optional<any Actor>, #Optional.some!enumelt, [[EXISTENTIAL]]
// CHECK: } // end sil function '$s4main17closureIsolatedToyyxms11GlobalActorRzlFyy{{(Yb)?}}xYccfU_'
func closureIsolatedTo<G: GlobalActor>(_: G.Type) {
  let _: @isolated(G) () -> Void = { _ = #isolation }
}
