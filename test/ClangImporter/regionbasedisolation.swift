// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/regionbasedisolation.h -verify -verify-additional-prefix ni- -c -swift-version 6
// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/regionbasedisolation.h -verify -verify-additional-prefix ni-ns- -c -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault
// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/regionbasedisolation.h -emit-silgen -swift-version 6 | %FileCheck %s

// REQUIRES: objc_interop

extension ObjCObject {
  // CHECK-LABEL: sil hidden [ossa] @$sSo10ObjCObjectC20regionbasedisolationE11sendObjectsSaySo8NSObjectCGyYaKF : $@convention(method) @async (@guaranteed ObjCObject) -> (@sil_sending @owned Array<NSObject>, @error any Error) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $ObjCObject):

  // Our result.
  // CHECK: [[RESULT:%.*]] = alloc_stack $Array<NSObject>

  // Our method.
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]], #ObjCObject.loadObjects!foreign : (ObjCObject) -> () async throws -> [NSObject], $@convention(objc_method) (@convention(block) @Sendable (Optional<NSArray>, Optional<NSError>) -> (), ObjCObject) -> ()

  // Begin setting up the unsafe continuation for our method. Importantly note
  // that [[UNSAFE_CONT]] is Sendable, so we lose any connection from the
  // continuation addr to any uses of the UnsafeContinuation.
  //
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr [throws] Array<NSObject>, [[RESULT]]
  // CHECK: [[UNSAFE_CONT:%.*]] = struct $UnsafeContinuation<Array<NSObject>, any Error> ([[CONT]])

  // Then prepare the block storage.
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
  // CHECK: [[PROJECT_BLOCK_STORAGE:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: [[EXISTENTIAL_BLOCK_STORAGE:%.*]] = init_existential_addr [[PROJECT_BLOCK_STORAGE]]

  // Then create a checked continuation from the unsafe continuation.
  //
  // CHECK: [[CREATE_CHECKED_CONT:%.*]] = function_ref @$ss34_createCheckedThrowingContinuationyScCyxs5Error_pGSccyxsAB_pGnlF : $@convention(thin) <τ_0_0> (UnsafeContinuation<τ_0_0, any Error>) -> @out CheckedContinuation<τ_0_0, any Error>
  // CHECK: [[CHECKED_CONT:%.*]] = alloc_stack $CheckedContinuation<Array<NSObject>, any Error>
  // CHECK: apply [[CREATE_CHECKED_CONT]]<Array<NSObject>>([[CHECKED_CONT]], [[UNSAFE_CONT]])

  // Then place the checked continuation into the block storage and perform a
  // merge_isolation_region in between the block storage and the result to
  // propagate that the result and the block storage are apart of the same
  // region despite the UnsafeContinuation blocking the relation in between
  // them.
  //
  // CHECK: copy_addr [take] [[CHECKED_CONT]] to [init] [[EXISTENTIAL_BLOCK_STORAGE]]
  // CHECK: merge_isolation_region [[BLOCK_STORAGE]], [[RESULT]]

  // Then create the actual block. NOTE: Since the block is @Sendable, the block
  // does not propagate regions.
  //
  // CHECK: [[COMPLETION_HANDLER_BLOCK:%.*]] = function_ref @$sSo7NSArrayCSgSo7NSErrorCSgIeyBhyy_SaySo8NSObjectCGTz_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any, Optional<NSArray>, Optional<NSError>) -> ()
  // CHECK: [[COMPLETION_BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]], invoke [[COMPLETION_HANDLER_BLOCK]]
  //
  // Since the block is @Sendable, it does not propagate the connection in
  // between self and the block storage when we just call the method. Thus we
  // need to perform a merge_isolation_region to communicate that the block
  // storage and self are part of the same region.
  //
  // CHECK: merge_isolation_region [[SELF]], [[BLOCK_STORAGE]]
  //
  // Then call the method.
  // CHECK: apply [[METHOD]]([[COMPLETION_BLOCK]], [[SELF]])
  // CHECK: } // end sil function '$sSo10ObjCObjectC20regionbasedisolationE11sendObjectsSaySo8NSObjectCGyYaKF'

  func sendObjects() async throws -> sending [NSObject] {
    // We emit an error since loadObjects just returns an [NSObject], not a
    // sending [NSObject].
    try await loadObjects()
  } // expected-error {{task or actor isolated value cannot be sent}}

  // Check if we do not mark the block as NS_SWIFT_SENDABLE
  //
  // CHECK-LABEL: sil hidden [ossa] @$sSo10ObjCObjectC20regionbasedisolationE12sendObjects2SaySo8NSObjectCGyYaKF : $@convention(method) @async (@guaranteed ObjCObject) -> (@sil_sending @owned Array<NSObject>, @error any Error) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $ObjCObject):

  // Our result.
  // CHECK: [[RESULT:%.*]] = alloc_stack $Array<NSObject>

  // Our method.
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]], #ObjCObject.loadObjects2!foreign : (ObjCObject) -> () async throws -> [NSObject], $@convention(objc_method) (@convention(block) @Sendable (Optional<NSArray>, Optional<NSError>) -> (), ObjCObject) -> ()

  // Begin setting up the unsafe continuation for our method. Importantly note
  // that [[UNSAFE_CONT]] is Sendable, so we lose any connection from the
  // continuation addr to any uses of the UnsafeContinuation.
  //
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr [throws] Array<NSObject>, [[RESULT]]
  // CHECK: [[UNSAFE_CONT:%.*]] = struct $UnsafeContinuation<Array<NSObject>, any Error> ([[CONT]])

  // Then prepare the block storage.
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
  // CHECK: [[PROJECT_BLOCK_STORAGE:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: [[EXISTENTIAL_BLOCK_STORAGE:%.*]] = init_existential_addr [[PROJECT_BLOCK_STORAGE]]

  // Then create a checked continuation from the unsafe continuation.
  //
  // CHECK: [[CREATE_CHECKED_CONT:%.*]] = function_ref @$ss34_createCheckedThrowingContinuationyScCyxs5Error_pGSccyxsAB_pGnlF : $@convention(thin) <τ_0_0> (UnsafeContinuation<τ_0_0, any Error>) -> @out CheckedContinuation<τ_0_0, any Error>
  // CHECK: [[CHECKED_CONT:%.*]] = alloc_stack $CheckedContinuation<Array<NSObject>, any Error>
  // CHECK: apply [[CREATE_CHECKED_CONT]]<Array<NSObject>>([[CHECKED_CONT]], [[UNSAFE_CONT]])

  // Then place the checked continuation into the block storage and perform a
  // merge_isolation_region in between the block storage and the result to
  // propagate that the result and the block storage are apart of the same
  // region despite the UnsafeContinuation blocking the relation in between
  // them.
  //
  // CHECK: copy_addr [take] [[CHECKED_CONT]] to [init] [[EXISTENTIAL_BLOCK_STORAGE]]
  // CHECK: merge_isolation_region [[BLOCK_STORAGE]], [[RESULT]]

  // Then create the actual block. NOTE: Since the block is @Sendable, the block
  // does not propagate regions.
  //
  // CHECK: [[COMPLETION_HANDLER_BLOCK:%.*]] = function_ref @$sSo7NSArrayCSgSo7NSErrorCSgIeyBhyy_SaySo8NSObjectCGTz_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any, Optional<NSArray>, Optional<NSError>) -> ()
  // CHECK: [[COMPLETION_BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]], invoke [[COMPLETION_HANDLER_BLOCK]]
  //
  // Since the block is @Sendable, it does not propagate the connection in
  // between self and the block storage when we just call the method. Thus we
  // need to perform a merge_isolation_region to communicate that the block
  // storage and self are part of the same region.
  //
  // CHECK: merge_isolation_region [[SELF]], [[BLOCK_STORAGE]]
  //
  // Then call the method.
  // CHECK: apply [[METHOD]]([[COMPLETION_BLOCK]], [[SELF]])
  // CHECK: } // end sil function '$sSo10ObjCObjectC20regionbasedisolationE12sendObjects2SaySo8NSObjectCGyYaKF'
  func sendObjects2() async throws -> sending [NSObject] {
    // We emit an error since loadObjects just returns an [NSObject], not a
    // sending [NSObject].
    try await loadObjects2()
  } // expected-error {{task or actor isolated value cannot be sent}}
}

@concurrent func useValueConcurrently<T>(_ t: T) async {}
@MainActor func useValueMainActor<T>(_ t: T) async {}
nonisolated(nonsending) func useValueNonIsolatedNonSending<T>(_ t: T) async {}

@MainActor func testMainActorMerging(_ y: NSObject) async {
  let x = ObjCObject()
  await x.useValue(y)
  await useValueConcurrently(x)  // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending main actor-isolated 'x' to nonisolated global function 'useValueConcurrently' risks causing data races between nonisolated and main actor-isolated uses}}
}

func testTaskLocal(_ y: NSObject) async {
  let x = ObjCObject()
  await x.useValue(y)
  await useValueConcurrently(x) // expected-ni-ns-error {{sending 'x' risks causing data races}}
  // expected-ni-ns-note @-1 {{sending task-isolated 'x' to nonisolated global function 'useValueConcurrently' risks causing data races between nonisolated and task-isolated uses}}

  // This is not safe since we merge x into y's region making x task
  // isolated. We then try to send it to a main actor function.
  await useValueMainActor(x) // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending task-isolated 'x' to main actor-isolated global function 'useValueMainActor' risks causing data races between main actor-isolated and task-isolated uses}}
}

actor MyActor {
  var field = NSObject()

  // This is safe since x.useValue is going to be nonisolated(nonsending) so we
  // are going to merge x into the actor. And useValueNonIsolatedNonSending
  // inherits from the context.
  func test() async {
    let x = ObjCObject()
    await x.useValue(field)
    await useValueNonIsolatedNonSending(x)
  }  
}
