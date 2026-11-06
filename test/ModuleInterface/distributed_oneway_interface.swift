// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Lib.swiftmodule -emit-module-interface-path %t/Lib.swiftinterface -language-mode 5 -parse-as-library -disable-availability-checking -enable-experimental-feature SendCalls -enable-library-evolution -module-name Lib -DLIB %s
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/Lib.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Lib.swiftinterface -module-name Lib -disable-availability-checking

// A client which only sees the module interface calls the thunks of the
// library
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -disable-availability-checking -enable-experimental-feature SendCalls -I %t -module-name main -DCLIENT %s | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls
// REQUIRES: OS=macosx

import Distributed

#if LIB

public distributed actor Worker {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  // INTERFACE: distributed public func plain(_ n: Swift::Int)
  public distributed func plain(_ n: Int) {}

  // INTERFACE: @oneway distributed public func notify(_ n: Swift::Int)
  @oneway public distributed func notify(_ n: Int) {}

  // INTERFACE: @oneway distributed public func notifyAsync(_ n: Swift::Int) async
  @oneway public distributed func notifyAsync(_ n: Int) async {}
}

// INTERFACE: public protocol Notifying : Distributed::DistributedActor
public protocol Notifying: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  // INTERFACE: distributed func plain()
  distributed func plain()
  // INTERFACE: @oneway distributed func notify()
  @oneway distributed func notify()
}

// INTERFACE: public protocol Pinging : _Concurrency::Actor
public protocol Pinging: Actor {
  // INTERFACE: @oneway func ping()
  @oneway func ping()
}

#elseif CLIENT

import Lib

func call(_ w: Worker) async throws {
  try await w.plain(1)
  try send w.notify(2)
  try send w.notifyAsync(3)
}

// CLIENT-LABEL: sil hidden [ossa] @$s4main4call{{.*}}6WorkerCYaKF
// CLIENT: function_ref @$s3Lib6WorkerC5plainyySiYaKFTE
// CLIENT: function_ref @$s3Lib6WorkerC6notifyyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Worker) -> @error any Error
// CLIENT: function_ref @$s3Lib6WorkerC11notifyAsyncyySiKFTE : $@convention(method) (@sil_sending Int, @guaranteed Worker) -> @error any Error

#endif
