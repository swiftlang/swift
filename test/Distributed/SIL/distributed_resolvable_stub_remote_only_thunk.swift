// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -module-name main %s | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: concurrency
// REQUIRES: distributed

// The distributed thunks of a '@Resolvable' stub '$Greeter' only keep the
// remote branch and trap if the stub is local, without calling into the stub
// body. A regular distributed actor keeps both branches

import Distributed

@Resolvable
protocol Greeter: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed func greet(name: String) -> String
  distributed func ping()
  distributed var count: Int { get }
}

distributed actor GreeterImpl: Greeter {
  distributed func greet(name: String) -> String {
    "Hello, \(name)!"
  }

  distributed func ping() {}

  distributed var count: Int {
    42
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Stub thunks only keep the remote branch

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: string_literal utf8 "greet(name:)"
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: function_ref @$s11Distributed26_distributedStubFatalError8functions5NeverOSS_tF
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: } // end sil function '$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5greet4nameS2S_tYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: string_literal utf8 "ping()"
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: function_ref @$s11Distributed26_distributedStubFatalError8functions5NeverOSS_tF
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: } // end sil function '$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE4pingyyYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5countSiyYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: string_literal utf8 "count"
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: function_ref @$s11Distributed26_distributedStubFatalError8functions5NeverOSS_tF
// CHECK-NOT: function_ref @$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE
// CHECK-NOT: witness_method $Self, #Greeter.
// CHECK: } // end sil function '$s4main7GreeterPAA11Distributed01_C9ActorStubRzrlE5countSiyYaKFTE'

// ==== ------------------------------------------------------------------------
// MARK: Regular distributed actor thunks keep the local branch

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main11GreeterImplC5greet4nameS2S_tYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK: function_ref @$s4main11GreeterImplC5greet4nameS2S_tF
// CHECK: } // end sil function '$s4main11GreeterImplC5greet4nameS2S_tYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main11GreeterImplC4pingyyYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK: function_ref @$s4main11GreeterImplC4pingyyF
// CHECK: } // end sil function '$s4main11GreeterImplC4pingyyYaKFTE'

// CHECK-LABEL: sil hidden [thunk] [distributed] {{.*}} @$s4main11GreeterImplC5countSiyYaKFTE :
// CHECK: function_ref @swift_distributed_actor_is_remote
// CHECK: function_ref @$s4main11GreeterImplC5countSivg
// CHECK: } // end sil function '$s4main11GreeterImplC5countSiyYaKFTE'
