// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -Xllvm -sil-full-demangle -target %target-swift-5.1-abi-triple -enable-experimental-async-top-level %s | %FileCheck %s

// a
// CHECK-LABEL: sil_global hidden @$s24toplevel_globalactorvars1aSivp : $Int

// CHECK-LABEL: sil private [ossa] @async_Main
// CHECK: bb0:
// CHECK-NEXT: [[MAIN:%.*]] = builtin "buildMainActorExecutorRef"() : $Builtin.Executor
// CHECK-NEXT: [[MAIN_OPTIONAL:%[0-9]+]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[MAIN]]

actor MyActorImpl {}

@globalActor
struct MyActor {
    static let shared = MyActorImpl()
}

var a = 10

// a initialization
// CHECK: alloc_global @$s24toplevel_globalactorvars1aSivp
// CHECK: [[AREF:%[0-9]+]] = global_addr @$s24toplevel_globalactorvars1aSivp
// CHECK: [[TEN_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 10
// CHECK: [[INT_TYPE:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[INT_INIT:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK: [[TEN:%[0-9]+]] = apply [[INT_INIT]]([[TEN_LIT]], [[INT_TYPE]])
// CHECK: store [[TEN]] to [trivial] [[AREF]]

@MyActor
func printFromMyActor(value : Int) {
    print(value)
}

print(a)

// print
// CHECK-NOT: hop_to_executor

// CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
// CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
// CHECK: end_access [[AACCESS]]
// CHECK-NOT: hop_to_executor

a += 1

// CHECK: [[ONE_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
// CHECK: [[INT_TYPE:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[INT_INIT:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK: [[ONE:%[0-9]+]] = apply [[INT_INIT]]([[ONE_LIT]], [[INT_TYPE]])
// CHECK-NOT: hop_to_executor
// CHECK: [[AACCESS:%[0-9]+]] = begin_access [modify] [dynamic] [[AREF]] : $*Int
// static Int.+= infix(_:_:)
// CHECK: [[PE_INT_FUNC:%[0-9]+]] = function_ref @$sSi2peoiyySiz_SitFZ
// CHECK: [[INCREMENTED:%[0-9]+]] = apply [[PE_INT_FUNC]]([[AACCESS]], [[ONE]], {{%[0-9]+}})
// CHECK: end_access [[AACCESS]]
// CHECK-NOT: hop_to_executor


await printFromMyActor(value: a)

// CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
// CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
// CHECK: end_access [[AACCESS]]

// CHECK: [[PRINTFROMMYACTOR_FUNC:%[0-9]+]] = function_ref @$s24toplevel_globalactorvars16printFromMyActor5valueySi_tF
// CHECK: [[ACTORREF:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MyActorImpl
// CHECK: hop_to_executor [[ACTORREF]] : $MyActorImpl
// CHECK: end_borrow [[ACTORREF]]
// CHECK: {{%[0-9]+}} = apply [[PRINTFROMMYACTOR_FUNC]]([[AGLOBAL]])
// CHECK: hop_to_executor [[MAIN_OPTIONAL]]

if a < 10 {
// CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
// CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
// CHECK: end_access [[AACCESS]]

// CHECK: [[TEN_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 10
// CHECK: [[INT_TYPE:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[INT_INIT:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
// CHECK: [[TEN:%[0-9]+]] = apply [[INT_INIT]]([[TEN_LIT]], [[INT_TYPE]])
// function_ref static Swift.Int.< infix(Swift.Int, Swift.Int) -> Swift.Bool
// CHECK: [[LESS_FUNC:%[0-9]+]] = function_ref @$sSi1loiySbSi_SitFZ
// CHECK: [[WRAPPED_COND:%[0-9]+]] = apply [[LESS_FUNC]]([[AGLOBAL]], [[TEN]], {{%[0-9]+}})
// CHECK: [[COND:%[0-9]+]] = struct_extract [[WRAPPED_COND]]
// CHECK: cond_br [[COND]], bb1, bb2
// CHECK: bb1:

    print(a)

    // print
    // CHECK-NOT: hop_to_executor

    // CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
    // CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
    // CHECK: end_access [[AACCESS]]
    // CHECK-NOT: hop_to_executor

    a += 1

    // CHECK: [[ONE_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
    // CHECK: [[INT_TYPE:%[0-9]+]] = metatype $@thin Int.Type
    // CHECK: [[INT_INIT:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC
    // CHECK: [[ONE:%[0-9]+]] = apply [[INT_INIT]]([[ONE_LIT]], [[INT_TYPE]])
    // CHECK-NOT: hop_to_executor
    // CHECK: [[AACCESS:%[0-9]+]] = begin_access [modify] [dynamic] [[AREF]] : $*Int
    // static Int.+= infix(_:_:)
    // CHECK: [[PE_INT_FUNC:%[0-9]+]] = function_ref @$sSi2peoiyySiz_SitFZ
    // CHECK: [[INCREMENTED:%[0-9]+]] = apply [[PE_INT_FUNC]]([[AACCESS]], [[ONE]], {{%[0-9]+}})
    // CHECK: end_access [[AACCESS]]
    // CHECK-NOT: hop_to_executor


    await printFromMyActor(value: a)

    // CHECK: [[AACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[AREF]] : $*Int
    // CHECK: [[AGLOBAL:%[0-9]+]] = load [trivial] [[AACCESS]] : $*Int
    // CHECK: end_access [[AACCESS]]

    // CHECK: [[PRINTFROMMYACTOR_FUNC:%[0-9]+]] = function_ref @$s24toplevel_globalactorvars16printFromMyActor5valueySi_tF
    // CHECK: [[ACTORREF:%[0-9]+]] = begin_borrow {{%[0-9]+}} : $MyActorImpl
    // CHECK: hop_to_executor [[ACTORREF]] : $MyActorImpl
    // CHECK: end_borrow [[ACTORREF]]
    // CHECK: {{%[0-9]+}} = apply [[PRINTFROMMYACTOR_FUNC]]([[AGLOBAL]])
    // CHECK: hop_to_executor [[MAIN_OPTIONAL]]
}
