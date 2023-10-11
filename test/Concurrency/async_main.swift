// RUN: %target-swift-frontend -emit-sil -disable-availability-checking -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-build-swift  -Xfrontend -disable-availability-checking -Xfrontend -parse-as-library %s -o %t_binary
// RUN: %target-codesign %t_binary
// RUN: %target-run %t_binary | %FileCheck %s --check-prefix=CHECK-EXEC

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@MainActor
var foo: Int = 42

func asyncFunc() async {
  print("Hello World!")
}

@main struct MyProgram {
  static func main() async throws {
    print("\(foo)")
    foo += 1
    await asyncFunc()
    print("\(foo)")
  }
}

// CHECK-EXEC: 42
// CHECK-EXEC-NEXT: Hello World!
// CHECK-EXEC-NEXT: 43

// static MyProgram.main()
// CHECK-SIL-LABEL: sil hidden @$s10async_main9MyProgramV0B0yyYaKFZ : $@convention(method) @async (@thin MyProgram.Type) -> @error any Error


// static MyProgram.$main()
// CHECK-SIL-LABEL: sil hidden @$s10async_main9MyProgramV5$mainyyYaKFZ : $@convention(method) @async (@thin MyProgram.Type) -> @error any Error


// async_Main
// CHECK-SIL-LABEL: sil private @async_Main : $@convention(thin) @async () -> () {
// call main
// CHECK-SIL:  %0 = metatype $@thin MyProgram.Type             // user: %2
// CHECK-SIL-NEXT:  // function_ref static MyProgram.$main()
// CHECK-SIL-NEXT:  %1 = function_ref @$s10async_main9MyProgramV5$mainyyYaKFZ : $@convention(method) @async (@thin MyProgram.Type) -> @error any Error // user: %2
// CHECK-SIL-NEXT:  try_apply %1(%0) : $@convention(method) @async (@thin MyProgram.Type) -> @error any Error, normal bb1, error bb2 // id: %2

// unwrap error and exit or explode
// CHECK-SIL: bb1(%3 : $()):
// CHECK-SIL-NEXT:  %4 = integer_literal $Builtin.Int32, 0
// CHECK-SIL-NEXT:  %5 = struct $Int32 (%4 : $Builtin.Int32)
// CHECK-SIL-NEXT:  // function_ref exit
// CHECK-SIL-NEXT:  %6 = function_ref @exit : $@convention(c) (Int32) -> Never
// CHECK-SIL-NEXT:  %7 = apply %6(%5) : $@convention(c) (Int32) -> Never
// CHECK-SIL-NEXT:  unreachable

// CHECK-SIL: bb2(%9 : $any Error):
// CHECK-SIL-NEXT:  %10 = builtin "errorInMain"(%9 : $any Error) : $()
// CHECK-SIL-NEXT:  unreachable

// main
// CHECK-SIL-LABEL: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {

// CHECK-SIL:       // function_ref async_Main
// CHECK-SIL-NEXT:  %2 = function_ref @async_Main : $@convention(thin) @async () -> ()
// CHECK-SIL-NEXT:  %3 = integer_literal $Builtin.Int64, 2048
// CHECK-SIL-NEXT:  %4 = struct $Int (%3 : $Builtin.Int64)
// CHECK-SIL-NEXT:  %5 = metatype $@thick ().Type
// CHECK-SIL-NEXT:  %6 = init_existential_metatype %5 : $@thick ().Type, $@thick any Any.Type
// CHECK-SIL-NEXT:  // function_ref thunk for @escaping @convention(thin) @async () -> ()
// CHECK-SIL-NEXT:  %7 = function_ref @$sIetH_yts5Error_pIegHrzo_TR : $@convention(thin) @async (@convention(thin) @async () -> ()) -> (@out (), @error any Error)
// CHECK-SIL-NEXT:  %8 = partial_apply [callee_guaranteed] %7(%2) : $@convention(thin) @async (@convention(thin) @async () -> ()) -> (@out (), @error any Error)
// CHECK-SIL-NEXT:  %9 = convert_function %8 : $@async @callee_guaranteed () -> (@out (), @error any Error) to $@async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <()>
// CHECK-SIL-NEXT:  %10 = builtin "createAsyncTask"<()>(%4 : $Int, %6 : $@thick any Any.Type, %9 : $@async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <()>) : $(Builtin.NativeObject, Builtin.RawPointer)
// CHECK-SIL-NEXT:  %11 = tuple_extract %10 : $(Builtin.NativeObject, Builtin.RawPointer), 0
// CHECK-SIL-NEXT:  // function_ref swift_job_run
// CHECK-SIL-NEXT:  %12 = function_ref @swift_job_run : $@convention(thin) (UnownedJob, UnownedSerialExecutor) -> ()
// CHECK-SIL-NEXT:  %13 = builtin "convertTaskToJob"(%11 : $Builtin.NativeObject) : $Builtin.Job
// CHECK-SIL-NEXT:  %14 = struct $UnownedJob (%13 : $Builtin.Job)
// CHECK-SIL-NEXT:  // function_ref swift_task_getMainExecutor
// CHECK-SIL-NEXT:  %15 = function_ref @swift_task_getMainExecutor : $@convention(thin) () -> Builtin.Executor
// CHECK-SIL-NEXT:  %16 = apply %15() : $@convention(thin) () -> Builtin.Executor
// CHECK-SIL-NEXT:  %17 = struct $UnownedSerialExecutor (%16 : $Builtin.Executor)
// CHECK-SIL-NEXT:  %18 = apply %12(%14, %17) : $@convention(thin) (UnownedJob, UnownedSerialExecutor) -> ()
// CHECK-SIL-NEXT:  // function_ref swift_task_asyncMainDrainQueue
// CHECK-SIL-NEXT:  %19 = function_ref @swift_task_asyncMainDrainQueue : $@convention(thin) () -> Never
// CHECK-SIL-NEXT:  %20 = apply %19() : $@convention(thin) () -> Never
// CHECK-SIL-NEXT:  unreachable
