// RUN: %target-swift-emit-silgen %s -target %target-swift-5.7-abi-triple | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-swift-5.7-abi-triple | %FileCheck %s --check-prefix=NOAVAIL

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: OS=macosx

// The distributed thunk emits tracing calls on its remote branch: an interval
// around encoding the invocation, and an interval around the whole 'remoteCall'
// (the outbound remote-call interval), each closed on both the success and the
// throwing path.
//
// The tracing entry points are '@_alwaysEmitIntoClient' and do their own
// '#available' check for the tracing runtime, so the thunk calls them
// unconditionally and no availability check is emitted into the thunk itself.
// NOAVAIL-NOT: _stdlib_isOSVersionAtLeast

import Distributed

distributed actor DA {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  // CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s25distributed_thunk_tracing2DAC5greet4nameS2S_tYaKFTE
  // CHECK:       bb0({{%[0-9]+}} : @guaranteed $String, [[SELF:%[0-9]+]] : @guaranteed $DA):

  // The thunk only takes the remote branch when the actor is remote
  // CHECK:       {{%[0-9]+}} = function_ref @swift_distributed_actor_is_remote : $@convention(thin) (@guaranteed AnyObject) -> Bool
  // CHECK:       {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(thin) (@guaranteed AnyObject) -> Bool
  // CHECK:       cond_br {{%[0-9]+}}, [[REMOTE_BB:bb[0-9]+]], {{bb[0-9]+}}

  // === The encode span ID lives in a 'var' box, so the 'catch' further down
  // reads back the very value the interval was opened with
  // CHECK:       [[ENCODE_SPAN_BOX:%[0-9]+]] = alloc_box ${ var UInt64 }
  // CHECK:       [[ENCODE_SPAN_BORROW:%[0-9]+]] = begin_borrow [var_decl] [[ENCODE_SPAN_BOX]]
  // CHECK:       [[ENCODE_SPAN_ADDR:%[0-9]+]] = project_box [[ENCODE_SPAN_BORROW]], 0

  // The encode interval opens with the target's mangled accessor record name and
  // the argument count, and is handed 'self' as the actor being called
  // CHECK:       {{%[0-9]+}} = string_literal utf8 "$s25distributed_thunk_tracing2DAC5greet4nameS2S_tYaKFTE"
  // CHECK:       [[ENCODE_BEGIN_FN:%[0-9]+]] = function_ref @$s11Distributed06_traceA20EncodeArgumentsBegin11targetActor0F10Identifier13argumentCounts6UInt64Vx_SSSitAA0aG0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : DistributedActor> (@guaranteed τ_0_0, @guaranteed String, Int) -> UInt64
  // CHECK:       [[ENCODE_SPAN:%[0-9]+]] = apply [[ENCODE_BEGIN_FN]]<DA>([[SELF]], {{%[0-9]+}}, {{%[0-9]+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : DistributedActor> (@guaranteed τ_0_0, @guaranteed String, Int) -> UInt64
  // CHECK:       store [[ENCODE_SPAN]] to [trivial] [[ENCODE_SPAN_ADDR]]

  // === The encoding sits inside the interval, and each 'record...' call gets
  // its own error edge out of it
  // CHECK:       [[RECORD_ARG:%[0-9]+]] = function_ref @$s11Distributed29LocalTestingInvocationEncoderV14recordArgumentyyAA010RemoteCallG0VyxGKSeRzSERzlF : $@convention(method) <τ_0_0 where τ_0_0 : Decodable, τ_0_0 : Encodable> (@in_guaranteed RemoteCallArgument<τ_0_0>, @inout LocalTestingInvocationEncoder) -> @error any Error
  // CHECK:       try_apply [[RECORD_ARG]]<String>({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) <τ_0_0 where τ_0_0 : Decodable, τ_0_0 : Encodable> (@in_guaranteed RemoteCallArgument<τ_0_0>, @inout LocalTestingInvocationEncoder) -> @error any Error, normal {{bb[0-9]+}}, error [[ARG_ERR_BB:bb[0-9]+]]

  // CHECK:       [[RECORD_RET:%[0-9]+]] = function_ref @$s11Distributed29LocalTestingInvocationEncoderV16recordReturnTypeyyxmKSeRzSERzlF : $@convention(method) <τ_0_0 where τ_0_0 : Decodable, τ_0_0 : Encodable> (@thick τ_0_0.Type, @inout LocalTestingInvocationEncoder) -> @error any Error
  // CHECK:       try_apply [[RECORD_RET]]<String>({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) <τ_0_0 where τ_0_0 : Decodable, τ_0_0 : Encodable> (@thick τ_0_0.Type, @inout LocalTestingInvocationEncoder) -> @error any Error, normal {{bb[0-9]+}}, error [[RET_ERR_BB:bb[0-9]+]]

  // CHECK:       [[DONE_REC:%[0-9]+]] = function_ref @$s11Distributed29LocalTestingInvocationEncoderV13doneRecordingyyKF : $@convention(method) (@inout LocalTestingInvocationEncoder) -> @error any Error
  // CHECK:       try_apply [[DONE_REC]]({{%[0-9]+}}) : $@convention(method) (@inout LocalTestingInvocationEncoder) -> @error any Error, normal {{bb[0-9]+}}, error [[DONE_ERR_BB:bb[0-9]+]]

  // === Encoding done: the span ID is read back out of the box and the encode
  // interval is closed with no error, so the trace records it as a success
  // CHECK:       [[ENCODE_SPAN_READ:%[0-9]+]] = begin_access [read] [unknown] [[ENCODE_SPAN_ADDR]]
  // CHECK:       [[ENCODE_SPAN_VAL:%[0-9]+]] = load [trivial] [[ENCODE_SPAN_READ]]
  // CHECK:       [[ENCODE_NO_ERROR:%[0-9]+]] = enum $Optional<any Error>, #Optional.none!enumelt
  // CHECK:       [[ENCODE_FAILED_DFLT:%[0-9]+]] = function_ref @$s11Distributed06_traceA18EncodeArgumentsEnd_5error6failedys6UInt64V_s5Error_pSgSbtFfA1_ : $@convention(thin) () -> Bool
  // CHECK:       [[ENCODE_FAILED:%[0-9]+]] = apply [[ENCODE_FAILED_DFLT]]() : $@convention(thin) () -> Bool
  // CHECK:       [[ENCODE_END_FN:%[0-9]+]] = function_ref @$s11Distributed06_traceA18EncodeArgumentsEnd_5error6failedys6UInt64V_s5Error_pSgSbtF : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()
  // CHECK:       {{%[0-9]+}} = apply [[ENCODE_END_FN]]([[ENCODE_SPAN_VAL]], [[ENCODE_NO_ERROR]], [[ENCODE_FAILED]]) : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()

  // === Then the outbound remote-call interval opens, with its own 'var' box so
  // the 'remoteCall' error path can close the very interval this opened
  // CHECK:       [[CALL_SPAN_BOX:%[0-9]+]] = alloc_box ${ var UInt64 }
  // CHECK:       [[CALL_SPAN_BORROW:%[0-9]+]] = begin_borrow [var_decl] [[CALL_SPAN_BOX]]
  // CHECK:       [[CALL_SPAN_ADDR:%[0-9]+]] = project_box [[CALL_SPAN_BORROW]], 0

  // It carries the same mangled name and 'self', and returns the span ID
  // CHECK:       {{%[0-9]+}} = string_literal utf8 "$s25distributed_thunk_tracing2DAC5greet4nameS2S_tYaKFTE"
  // CHECK:       [[CALL_BEGIN_FN:%[0-9]+]] = function_ref @$s11Distributed06_traceA10RemoteCall11targetActor0E10Identifiers6UInt64Vx_SStAA0aF0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : DistributedActor> (@guaranteed τ_0_0, @guaranteed String) -> UInt64
  // CHECK:       [[CALL_SPAN:%[0-9]+]] = apply [[CALL_BEGIN_FN]]<DA>([[SELF]], {{%[0-9]+}}) : $@convention(thin) <τ_0_0 where τ_0_0 : DistributedActor> (@guaranteed τ_0_0, @guaranteed String) -> UInt64
  // CHECK:       store [[CALL_SPAN]] to [trivial] [[CALL_SPAN_ADDR]]

  // ...and only then is 'remoteCall' invoked, on the same 'self', inside the
  // interval, with its own error edge
  // CHECK:       [[REMOTE_CALL:%[0-9]+]] = function_ref @$s11Distributed012LocalTestingA11ActorSystemC10remoteCall2on6target10invocation8throwing9returningq0_x_AA06RemoteG6TargetVAA0bC17InvocationEncoderVzq_mq0_mtYaKAA0aD0Rzs5ErrorR_SeR0_SER0_AA0bcD2IDV0R0Rtzr1_lF : $@convention(method) @async <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : DistributedActor, τ_0_1 : Error, τ_0_2 : Decodable, τ_0_2 : Encodable, τ_0_0.ID == LocalTestingActorID> (@guaranteed τ_0_0, @in_guaranteed RemoteCallTarget, @inout LocalTestingInvocationEncoder, @thick τ_0_1.Type, @thick τ_0_2.Type, @guaranteed LocalTestingDistributedActorSystem) -> (@out τ_0_2, @error any Error)
  // CHECK:       try_apply [[REMOTE_CALL]]<DA, Never, String>({{%[0-9]+}}, [[SELF]], {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}) : {{.*}}, normal [[CALL_OK_BB:bb[0-9]+]], error [[CALL_ERR_BB:bb[0-9]+]]

  // === Success: the remote-call span ID is read back out of its box and the
  // interval is closed with no error, then the thunk returns the result
  // CHECK:       [[CALL_OK_BB]]({{%[0-9]+}} : $()):
  // CHECK:       [[CALL_SPAN_READ:%[0-9]+]] = begin_access [read] [unknown] [[CALL_SPAN_ADDR]]
  // CHECK:       [[CALL_SPAN_VAL:%[0-9]+]] = load [trivial] [[CALL_SPAN_READ]]
  // CHECK:       [[CALL_NO_ERROR:%[0-9]+]] = enum $Optional<any Error>, #Optional.none!enumelt
  // CHECK:       [[CALL_FAILED_DFLT:%[0-9]+]] = function_ref @$s11Distributed06_traceA13RemoteCallEnd_5error6failedys6UInt64V_s5Error_pSgSbtFfA1_ : $@convention(thin) () -> Bool
  // CHECK:       [[CALL_FAILED:%[0-9]+]] = apply [[CALL_FAILED_DFLT]]() : $@convention(thin) () -> Bool
  // CHECK:       [[CALL_END_FN:%[0-9]+]] = function_ref @$s11Distributed06_traceA13RemoteCallEnd_5error6failedys6UInt64V_s5Error_pSgSbtF : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()
  // CHECK:       {{%[0-9]+}} = apply [[CALL_END_FN]]([[CALL_SPAN_VAL]], [[CALL_NO_ERROR]], [[CALL_FAILED]]) : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()

  // === Encoding failure: one shared catch block closes the encode interval,
  // reporting the error that was thrown so the trace carries its type
  // CHECK:       [[ENCODE_CATCH_BB:bb[0-9]+]]({{%[0-9]+}} : @owned $any Error):

  // The error is bound to a 'let' so it can be both reported and rethrown
  // CHECK:       {{%[0-9]+}} = move_value [lexical] [var_decl] {{%[0-9]+}}

  // The same encode span ID as above, read back out of the box
  // CHECK:       [[ENCODE_SPAN_READ2:%[0-9]+]] = begin_access [read] [unknown] [[ENCODE_SPAN_ADDR]]
  // CHECK:       [[ENCODE_SPAN_VAL2:%[0-9]+]] = load [trivial] [[ENCODE_SPAN_READ2]]
  // CHECK:       [[ENCODE_SOME_ERROR:%[0-9]+]] = enum $Optional<any Error>, #Optional.some!enumelt, {{%[0-9]+}}
  // CHECK:       [[ENCODE_FAILED_DFLT2:%[0-9]+]] = function_ref @$s11Distributed06_traceA18EncodeArgumentsEnd_5error6failedys6UInt64V_s5Error_pSgSbtFfA1_ : $@convention(thin) () -> Bool
  // CHECK:       [[ENCODE_FAILED2:%[0-9]+]] = apply [[ENCODE_FAILED_DFLT2]]() : $@convention(thin) () -> Bool
  // CHECK:       [[ENCODE_END_FN2:%[0-9]+]] = function_ref @$s11Distributed06_traceA18EncodeArgumentsEnd_5error6failedys6UInt64V_s5Error_pSgSbtF : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()
  // CHECK:       {{%[0-9]+}} = apply [[ENCODE_END_FN2]]([[ENCODE_SPAN_VAL2]], [[ENCODE_SOME_ERROR]], [[ENCODE_FAILED2]]) : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()

  // The encode interval is closed before the error is rethrown, never after
  // CHECK:       {{%[0-9]+}} = builtin "willThrow"({{%[0-9]+}}) : $()
  // CHECK:       br [[THROW_BB:bb[0-9]+]]({{%[0-9]+}})

  // ...and every 'record...' error edge branches into exactly that block
  // CHECK:       [[ARG_ERR_BB]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:       br [[ENCODE_CATCH_BB]]({{%[0-9]+}})
  // CHECK:       [[RET_ERR_BB]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:       br [[ENCODE_CATCH_BB]]({{%[0-9]+}})
  // CHECK:       [[DONE_ERR_BB]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:       br [[ENCODE_CATCH_BB]]({{%[0-9]+}})

  // === 'remoteCall' failure: its own catch block closes the remote-call
  // interval, reporting the thrown error's type, then rethrows
  // CHECK:       [[CALL_ERR_BB]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:       {{%[0-9]+}} = move_value [lexical] [var_decl] {{%[0-9]+}}
  // CHECK:       [[CALL_SPAN_READ2:%[0-9]+]] = begin_access [read] [unknown] [[CALL_SPAN_ADDR]]
  // CHECK:       [[CALL_SPAN_VAL2:%[0-9]+]] = load [trivial] [[CALL_SPAN_READ2]]
  // CHECK:       [[CALL_SOME_ERROR:%[0-9]+]] = enum $Optional<any Error>, #Optional.some!enumelt, {{%[0-9]+}}
  // CHECK:       [[CALL_FAILED_DFLT2:%[0-9]+]] = function_ref @$s11Distributed06_traceA13RemoteCallEnd_5error6failedys6UInt64V_s5Error_pSgSbtFfA1_ : $@convention(thin) () -> Bool
  // CHECK:       [[CALL_FAILED2:%[0-9]+]] = apply [[CALL_FAILED_DFLT2]]() : $@convention(thin) () -> Bool
  // CHECK:       [[CALL_END_FN2:%[0-9]+]] = function_ref @$s11Distributed06_traceA13RemoteCallEnd_5error6failedys6UInt64V_s5Error_pSgSbtF : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()
  // CHECK:       {{%[0-9]+}} = apply [[CALL_END_FN2]]([[CALL_SPAN_VAL2]], [[CALL_SOME_ERROR]], [[CALL_FAILED2]]) : $@convention(thin) (UInt64, @guaranteed Optional<any Error>, Bool) -> ()
  // CHECK:       {{%[0-9]+}} = builtin "willThrow"({{%[0-9]+}}) : $()
  // CHECK:       br [[THROW_BB]]({{%[0-9]+}})

  // The rethrow is shared by the encode and 'remoteCall' error paths
  // CHECK:       [[THROW_BB]]({{%[0-9]+}} : @owned $any Error):
  // CHECK:       throw {{%[0-9]+}}
  distributed func greet(name: String) -> String {
    return "Hello, \(name)!"
  }
}
