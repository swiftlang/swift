// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name remoteCall -primary-file %s -dump-ast -enable-experimental-distributed -disable-availability-checking -I %t | %FileCheck %s --enable-var-scope --color --dump-input=always
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor MyDistActor {

  distributed func funcNormal(param: String) -> String {
    param
  }

  // we only strictly check the first one as it is the most tricky
  // the other ones we check by means of "if synthesis is wrong, they'd fail to
//  // compile":
//
//  distributed func funcNoop() {
//  }
//
//  distributed func funcHi() -> String {
//    "Hello"
//  }
//
//  distributed func funcThrows(param: String) throws -> String {
//    param
//  }
//
//  distributed func funcThrows(one: String, two: Int) throws -> String {
//    one
//  }
//
//  distributed func funcAsync(param: String) async -> String {
//    param
//  }
//
//  distributed func funcAsyncThrows(param: String) async throws -> String {
//    param
//  }
}

// CHECK: (func_decl implicit "$dist_funcNormal(param:)" interface type='(MyDistActor) -> (String) async throws -> String' access=internal captures=(actorSystem<direct>) nonisolated
// CHECK:  (parameter known-to-be-local "self" type='MyDistActor' interface type='MyDistActor')
// CHECK:  (parameter_list
// CHECK:  (parameter "param" apiName=param type='String' interface type='String'))
// CHECK:  (brace_stmt implicit
// CHECK:    (if_stmt implicit
// CHECK:      (call_expr implicit type='Bool' nothrow
// CHECK:        (declref_expr implicit type='(AnyObject) -> Bool' decl=_Distributed.(file).__isRemoteActor function_ref=single)
// CHECK:        (argument_list implicit
// CHECK:          (argument
// CHECK:            (erasure_expr implicit type='AnyObject'
// CHECK:              (declref_expr implicit type='MyDistActor' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).self known-to-be-local function_ref=unapplied)))
// CHECK:        ))
// CHECK:      (brace_stmt implicit
// CHECK:        (pattern_binding_decl implicit
// CHECK:          (pattern_named implicit type='FakeActorSystem.InvocationEncoder' 'invocation')
// CHECK:          Processed init:
// CHECK:          (call_expr implicit type='FakeActorSystem.InvocationEncoder' nothrow
// CHECK:            (dot_syntax_call_expr implicit type='() -> FakeActorSystem.InvocationEncoder' nothrow
// CHECK:              (declref_expr implicit type='(FakeActorSystem) -> () -> FakeActorSystem.InvocationEncoder' decl=FakeDistributedActorSystems.(file).FakeActorSystem.makeInvocationEncoder() function_ref=single)
// CHECK:              (argument_list implicit
// CHECK:                (argument
// CHECK:                  (member_ref_expr implicit type='DefaultDistributedActorSystem' decl=remoteCall.(file).MyDistActor.actorSystem
// CHECK:                    (declref_expr implicit type='MyDistActor' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).self known-to-be-local function_ref=unapplied)))
// CHECK:              ))
// CHECK:            (argument_list implicit)))
// CHECK:        (var_decl implicit "invocation" type='FakeActorSystem.InvocationEncoder' interface type='FakeActorSystem.InvocationEncoder' access=private readImpl=stored writeImpl=stored readWriteImpl=stored)
// CHECK:        (try_expr implicit type='()'
// CHECK:          (call_expr implicit type='()' throws
// CHECK:            (dot_syntax_call_expr implicit type='() throws -> ()' nothrow
// CHECK:              (declref_expr implicit type='(inout FakeInvocationEncoder) -> () throws -> ()' decl=FakeDistributedActorSystems.(file).FakeInvocationEncoder.doneRecording() function_ref=single)
// CHECK:              (argument_list implicit
// CHECK:                (argument inout
// CHECK:                  (inout_expr implicit type='inout FakeInvocationEncoder'
// CHECK:                    (declref_expr implicit type='@lvalue FakeActorSystem.InvocationEncoder' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).invocation function_ref=unapplied)))
// CHECK:              ))
// CHECK:            (argument_list implicit)))
// CHECK:        (pattern_binding_decl implicit
// CHECK:          (pattern_named implicit type='RemoteCallTarget' 'target')
// CHECK:          Processed init:
// CHECK:          (call_expr implicit type='RemoteCallTarget' nothrow
// CHECK:            (constructor_ref_call_expr implicit type='(String) -> RemoteCallTarget' nothrow
// CHECK:              (declref_expr implicit type='(RemoteCallTarget.Type) -> (String) -> RemoteCallTarget' decl=_Distributed.(file).RemoteCallTarget.init(_mangledName:) function_ref=single)
// CHECK:              (argument_list implicit
// CHECK:                (argument
// CHECK:                  (type_expr implicit type='RemoteCallTarget.Type' typerepr='RemoteCallTarget'))
// CHECK:              ))
// CHECK:            (argument_list implicit labels=_mangledName:
// CHECK:              (argument label=_mangledName
// CHECK:                (string_literal_expr implicit type='String' encoding=utf8 value="[[MANGLED_TARGET_NAME:.*]]"
// CHECK:            )))
// CHECK:        (var_decl implicit "target" type='RemoteCallTarget' interface type='RemoteCallTarget' access=private let readImpl=stored immutable)
// CHECK:        (return_stmt implicit
// CHECK:          (try_expr implicit type='String'
// CHECK:            (await_expr implicit type='String'
// CHECK:              (call_expr implicit type='String' throws
// CHECK:                (dot_syntax_call_expr implicit type='(MyDistActor, RemoteCallTarget, inout FakeActorSystem.InvocationEncoder, Never.Type, String.Type) async throws -> String' nothrow
// CHECK:                   (declref_expr implicit type='(FakeActorSystem) -> (MyDistActor, RemoteCallTarget, inout FakeActorSystem.InvocationEncoder, Never.Type, String.Type) async throws -> String' decl=FakeDistributedActorSystems.(file).FakeActorSystem.remoteCall(on:target:invocation:throwing:returning:) [with (substitution_map generic_signature=<Act, Err, Res where Act : DistributedActor, Err : Error, Res : Decodable, Res : Encodable, Act.ID == FakeActorSystem.ActorID> (substitution Act -> MyDistActor) (substitution Err -> Never) (substitution Res -> String))] function_ref=single)
// CHECK:                  (argument_list implicit
// CHECK:                    (argument
// CHECK:                      (declref_expr implicit type='DefaultDistributedActorSystem' decl=remoteCall.(file).MyDistActor.actorSystem function_ref=unapplied))
// CHECK:                  ))
// CHECK:                (argument_list implicit labels=on:target:invocation:throwing:returning:
// CHECK:                  (argument label=on
// CHECK:                    (declref_expr implicit type='MyDistActor' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).self known-to-be-local function_ref=unapplied))
// CHECK:                  (argument label=target
// CHECK:                    (declref_expr implicit type='RemoteCallTarget' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).target function_ref=unapplied))
// CHECK:                  (argument label=invocation inout
// CHECK:                    (inout_expr implicit type='inout FakeActorSystem.InvocationEncoder'
// CHECK:                      (declref_expr implicit type='@lvalue FakeActorSystem.InvocationEncoder' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).invocation function_ref=unapplied)))
// CHECK:                  (argument label=throwing
// CHECK:                    (dot_self_expr type='Never.Type'
// CHECK:                      (type_expr implicit type='Never.Type' typerepr='Never')))
// CHECK:                ))))))
// CHECK:      (brace_stmt implicit
// CHECK:        (return_stmt implicit
// CHECK:          (await_expr implicit type='String'
// CHECK:            (call_expr implicit type='String' nothrow
// CHECK:              (dot_syntax_call_expr implicit type='(String) -> String' nothrow
// CHECK:                (declref_expr implicit type='(isolated MyDistActor) -> (String) -> String' decl=remoteCall.(file).MyDistActor.funcNormal(param:)@[[SOURCE_LOC:.*]] function_ref=single)
// CHECK:                (argument_list implicit
// CHECK:                  (argument
// CHECK:                    (declref_expr implicit type='MyDistActor' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).self known-to-be-local function_ref=unapplied))
// CHECK:                ))
// CHECK:                (argument_list implicit labels=param:
// CHECK:                  (argument label=param
// CHECK:                    (declref_expr implicit type='String' decl=remoteCall.(file).MyDistActor.$dist_funcNormal(param:).param function_ref=unapplied))
// CHECK:                ))))))))
