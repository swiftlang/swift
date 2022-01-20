// RUN: %target-swift-emit-silgen -disable-availability-checking -Xllvm -sil-full-demangle -enable-experimental-async-top-level %s | %FileCheck %s

// a
// CHECK-LABEL: sil_global hidden @$s24toplevel_globalactorvars1aSivp : $Int
// b
// CHECK-LABEL: sil_global hidden @$s24toplevel_globalactorvars1bSivp : $Int

// CHECK-LABEL: sil hidden [ossa] @async_Main
// CHECK: bb0:
// CHECK-NEXT: alloc_global @$s24toplevel_globalactorvars1aSivp
// CHECK-NEXT: [[AREF:%[0-9]+]] = global_addr @$s24toplevel_globalactorvars1aSivp : $*Int

actor MyActorImpl {}

@globalActor
struct MyActor {
    static let shared = MyActorImpl()
}

@MyActor
var a = 10

@MyActor
func incrementA() {
    a += 1
}

await print(a)

// CHECK: [[ACTORREF:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MyActorImpl
// CHECK: [[OLDACTOR:%[0-9]+]] = builtin "getCurrentExecutor"() : $Optional<Builtin.Executor>
// CHECK: hop_to_executor [[ACTORREF]] : $MyActorImpl
// CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
// CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
// CHECK: end_access [[AACCESS]]
// CHECK: hop_to_executor [[OLDACTOR]]
// CHECK: end_borrow [[ACTORREF]]

await incrementA()

// CHECK: [[INCREMENTA:%[0-9]+]] = function_ref @$s24toplevel_globalactorvars10incrementAyyF
// CHECK: [[ACTORREF:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MyActorImpl
// CHECK: [[OLDACTOR:%[0-9]+]] = builtin "getCurrentExecutor"() : $Optional<Builtin.Executor>
// CHECK: hop_to_executor [[ACTORREF]] : $MyActorImpl
// CHECK: {{%[0-9]+}} = apply [[INCREMENTA]]()
// CHECK: hop_to_executor [[OLDACTOR]]
// CHECK: end_borrow [[ACTORREF]]

await print(a)

// CHECK: [[ACTORREF:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MyActorImpl
// CHECK: [[OLDACTOR:%[0-9]+]] = builtin "getCurrentExecutor"() : $Optional<Builtin.Executor>
// CHECK: hop_to_executor [[ACTORREF]] : $MyActorImpl
// CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
// CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
// CHECK: end_access [[AACCESS]]
// CHECK: hop_to_executor [[OLDACTOR]]
// CHECK: end_borrow [[ACTORREF]]

var b = 11

// CHECK: alloc_global @$s24toplevel_globalactorvars1bSivp
// CHECK: [[BGLOBAL_ADDR:%[0-9]+]] = global_addr @$s24toplevel_globalactorvars1bSivp
// Int.init(_builtinIntegerLiteral:)
// CHECK: [[INT_INIT_FUNC:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK: [[INITD_INT:%[0-9]+]] = apply [[INT_INIT_FUNC]]({{%[0-9]+}}, {{%[0-9]+}})
// CHECK: store [[INITD_INT]] to [trivial] [[BGLOBAL_ADDR]]

b += 1

// CHECK-NOT: hop_to_executor
// CHECK: [[BACCESS:%[0-9]+]] = begin_access [modify] [dynamic] [[BGLOBAL_ADDR]]
// static Int.+= infix(_:_:)
// CHECK: [[PE_INT_FUNC:%[0-9]+]] = function_ref @$sSi2peoiyySiz_SitFZ
// CHECK: [[INCREMENTED:%[0-9]+]] = apply [[PE_INT_FUNC]]([[BACCESS]], {{%[0-9]+}}, {{%[0-9]+}})
// CHECK: end_access [[BACCESS]]
