// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -module-name main %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// Which of two overloads of an actor method, one of which is '@oneway', a call
// picks: the operand of a 'send' picks the '@oneway' overload, any other call
// picks the one which is not '@oneway'. This holds over the preference for an
// 'async' overload in an async context, since the operand of a 'send' is
// type-checked in an async closure and an awaited or plain call may be in an
// async function, and over the preference for an overload which needs no
// default arguments. Two functions which differ only in '@oneway' are
// redeclarations (see test/attr/attr_oneway.swift), so the overloads here
// differ in 'async' or in their parameters too.
//
// Each call is checked with an argument as well as without one: with a single
// unlabeled argument the constraint solver favors the overloads which match
// the argument's type exactly, and used to also drop the ones whose 'async'
// did not match the context, so that the other overload was never attempted
// and 'send self.tell(1)' called 'func tell(_:) async'.
//
// Each function makes one call; the operand of a 'send' is emitted in a
// closure ('fU_') of the function

// ==== -----------------------------------------------------------------------
// MARK: '@oneway func f(_:)' and 'func f(_:) async'

actor A1 {
  @oneway func tell(_ n: Int) {}
  func tell(_ n: Int) async {}
  @oneway func zero() {}
  func zero() async {}

  func insideSendArg() async { send self.tell(1) }
  func insideAwaitArg() async { await self.tell(1) }
  func insideSendZero() async { send self.zero() }
  func insideAwaitZero() async { await self.zero() }
}

func a1SendArg(_ a: A1) async { send a.tell(1) }
func a1SendFromSyncArg(_ a: A1) { send a.tell(1) }
func a1AwaitArg(_ a: A1) async { await a.tell(1) }
func a1SendZero(_ a: A1) async { send a.zero() }
func a1SendFromSyncZero(_ a: A1) { send a.zero() }
func a1AwaitZero(_ a: A1) async { await a.zero() }

// ==== -----------------------------------------------------------------------
// MARK: '@oneway func f(_:) async' and 'func f(_:)'

actor A2 {
  @oneway func tell(_ n: Int) async {}
  func tell(_ n: Int) {}
  @oneway func zero() async {}
  func zero() {}

  func insideSendArg() async { send self.tell(1) }
  func insideAwaitArg() async { await self.tell(1) } // expected-warning{{no 'async' operations occur within 'await' expression}}
  func insidePlainArg() async { self.tell(1) }
  func insidePlainSyncArg() { self.tell(1) }
  func insideSendZero() async { send self.zero() }
  func insideAwaitZero() async { await self.zero() } // expected-warning{{no 'async' operations occur within 'await' expression}}
  func insidePlainZero() async { self.zero() }
  func insidePlainSyncZero() { self.zero() }
}

func a2SendArg(_ a: A2) async { send a.tell(1) }
func a2SendFromSyncArg(_ a: A2) { send a.tell(1) }
func a2AwaitArg(_ a: A2) async { await a.tell(1) }
func a2SendZero(_ a: A2) async { send a.zero() }
func a2SendFromSyncZero(_ a: A2) { send a.zero() }
func a2AwaitZero(_ a: A2) async { await a.zero() }

// ==== -----------------------------------------------------------------------
// MARK: '@oneway func f(a:)' with a default argument and 'func f()'

actor A3 {
  @oneway func example(a: Int = 32) {}
  func example() {}

  func insideSend() async { send self.example() }
  func insideSendFromSync() { send self.example() }
  func insidePlain() { self.example() }
  func insidePlainAsync() async { self.example() }
  func insideSendLabeled() { send self.example(a: 1) }
}

func a3Send(_ a: A3) async { send a.example() }
func a3SendFromSync(_ a: A3) { send a.example() }
func a3Await(_ a: A3) async { await a.example() }
func a3SendLabeled(_ a: A3) { send a.example(a: 1) }

@MainActor @oneway func example(a: Int = 32) {}
@MainActor func example() {}

@MainActor func mainSend() { send example() }
@MainActor func mainPlain() { example() }
@MainActor func mainSendLabeled() { send example(a: 1) }
func nonisolatedAwait() async { await example() }
func nonisolatedSend() { send example() }

// CHECK-LABEL: sil private [ossa] @$s4main2A1C13insideSendArgyyYaF{{.*}}fU_ :
// CHECK: #A1.tell : (isolated A1) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A1C14insideAwaitArgyyYaF :
// CHECK: #A1.tell : (isolated A1) -> (Int) async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A1C14insideSendZeroyyYaF{{.*}}fU_ :
// CHECK: #A1.zero : (isolated A1) -> () -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A1C15insideAwaitZeroyyYaF :
// CHECK: #A1.zero : (isolated A1) -> () async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main9a1SendArgyyAA2A1CYaF{{.*}}fU_ :
// CHECK: #A1.tell : (isolated A1) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s4main17a1SendFromSyncArgyyAA2A1CF{{.*}}fU_ :
// CHECK: #A1.tell : (isolated A1) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main10a1AwaitArgyyAA2A1CYaF :
// CHECK: #A1.tell : (isolated A1) -> (Int) async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main10a1SendZeroyyAA2A1CYaF{{.*}}fU_ :
// CHECK: #A1.zero : (isolated A1) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s4main18a1SendFromSyncZeroyyAA2A1CF{{.*}}fU_ :
// CHECK: #A1.zero : (isolated A1) -> () -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main11a1AwaitZeroyyAA2A1CYaF :
// CHECK: #A1.zero : (isolated A1) -> () async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A2C13insideSendArgyyYaF{{.*}}fU_ :
// CHECK: #A2.tell : (isolated A2) -> (Int) async -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C14insideAwaitArgyyYaF :
// CHECK: #A2.tell : (isolated A2) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C14insidePlainArgyyYaF :
// CHECK: #A2.tell : (isolated A2) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C18insidePlainSyncArgyyF :
// CHECK: #A2.tell : (isolated A2) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A2C14insideSendZeroyyYaF{{.*}}fU_ :
// CHECK: #A2.zero : (isolated A2) -> () async -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C15insideAwaitZeroyyYaF :
// CHECK: #A2.zero : (isolated A2) -> () -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C15insidePlainZeroyyYaF :
// CHECK: #A2.zero : (isolated A2) -> () -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A2C19insidePlainSyncZeroyyF :
// CHECK: #A2.zero : (isolated A2) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s4main9a2SendArgyyAA2A2CYaF{{.*}}fU_ :
// CHECK: #A2.tell : (isolated A2) -> (Int) async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main17a2SendFromSyncArgyyAA2A2CF{{.*}}fU_ :
// CHECK: #A2.tell : (isolated A2) -> (Int) async -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main10a2AwaitArgyyAA2A2CYaF :
// CHECK: #A2.tell : (isolated A2) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s4main10a2SendZeroyyAA2A2CYaF{{.*}}fU_ :
// CHECK: #A2.zero : (isolated A2) -> () async -> ()
// CHECK-LABEL: sil private [ossa] @$s4main18a2SendFromSyncZeroyyAA2A2CF{{.*}}fU_ :
// CHECK: #A2.zero : (isolated A2) -> () async -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main11a2AwaitZeroyyAA2A2CYaF :
// CHECK: #A2.zero : (isolated A2) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A3C10insideSendyyYaF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A3C18insideSendFromSyncyyF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A3C11insidePlainyyF :
// CHECK: #A3.example : (isolated A3) -> () -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main2A3C16insidePlainAsyncyyYaF :
// CHECK: #A3.example : (isolated A3) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s4main2A3C17insideSendLabeledyyF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main6a3SendyyAA2A3CYaF :
// CHECK: function_ref @$s4main2A3C7example1aySi_tFfA_
// CHECK-LABEL: sil private [ossa] @$s4main6a3SendyyAA2A3CYaF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s4main14a3SendFromSyncyyAA2A3CF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main7a3AwaityyAA2A3CYaF :
// CHECK: #A3.example : (isolated A3) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s4main13a3SendLabeledyyAA2A3CF{{.*}}fU_ :
// CHECK: #A3.example : (isolated A3) -> (Int) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s4main0A4SendyyF :
// CHECK: function_ref @$s4main7example1aySi_tFfA_
// CHECK-LABEL: sil private [ossa] @$s4main0A4SendyyF{{.*}}fU_ :
// CHECK: function_ref @$s4main7example1aySi_tF :
// CHECK-LABEL: sil hidden [ossa] @$s4main0A5PlainyyF :
// CHECK: function_ref @$s4main7exampleyyF :
// CHECK-LABEL: sil private [ossa] @$s4main0A11SendLabeledyyF{{.*}}fU_ :
// CHECK: function_ref @$s4main7example1aySi_tF :
// CHECK-LABEL: sil hidden [ossa] @$s4main16nonisolatedAwaityyYaF :
// CHECK: function_ref @$s4main7exampleyyF :
// CHECK-LABEL: sil private [ossa] @$s4main15nonisolatedSendyyF{{.*}}fU_ :
// CHECK: function_ref @$s4main7example1aySi_tF :
