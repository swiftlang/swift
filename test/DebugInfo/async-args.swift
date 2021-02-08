// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M -enable-experimental-concurrency \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK \
// RUN:    --check-prefix=CHECK-%target-cpu
// REQUIRES: concurrency

func use<T>(_ t: T) {}
func forceSplit() async {
}
func withGenericArg<T>(_ msg: T) async {
  // This odd debug info is part of a contract with CoroSplit/CoroFrame to fix
  // this up after coroutine splitting.
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYlF"(%swift.task* %0, %swift.executor* %1, %swift.context* swiftasync %2)
  // CHECK: call void @llvm.dbg.declare(metadata %swift.context** %[[ALLOCA:[^,]+]],
  // CHECK-SAME:   metadata ![[TAU:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK: call void @llvm.dbg.declare(metadata %swift.context** %[[ALLOCA]],
  // CHECK-SAME:   metadata ![[MSG:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK: store %swift.context* %2, %swift.context** %[[ALLOCA]], align

  await forceSplit()
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYlF.resume.0"(i8* %0, i8* %1, i8* swiftasync %2)

  // CHECK-arm64e: [[CTXT_PTR:%[0-9]+]] = bitcast i8* %2 to i8**
  // CHECK-arm64e: [[SIGNED_CTXT:%[0-9]+]] = load i8*, i8** [[CTXT_PTR]]
  // CHECK-arm64e: [[CTXT_PTR_INT:%[0-9]+]] = ptrtoint i8** [[CTXT_PTR]] to i64
  // CHECK-arm64e: [[PTRAUTH_BLEND:%[0-9]+]] = call i64 @llvm.ptrauth.blend.i64(i64 [[CTXT_PTR_INT]], i64 48546)
  // CHECK-arm64e: [[SIGNED_CTXT_INT:%[0-9]+]] = ptrtoint i8* [[SIGNED_CTXT]] 
  // CHECK-arm64e: [[CTXT:%[0-9]+]] = call i64 @llvm.ptrauth.auth.i64(i64 [[SIGNED_CTXT_INT]], i32 2, i64 [[PTRAUTH_BLEND]])
  // CHECK-arm64e:   %[[ALLOCA:[0-9+]]] = inttoptr i64 [[CTXT]] to i8*
  // CHECK-arm64e: call void @llvm.dbg.declare(metadata i8* %[[ALLOCA]],
  // CHECK-arm64e-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-arm64e-SAME:     DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-arm64e-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-arm64e: call void @llvm.dbg.declare(metadata i8* %[[ALLOCA]],
  // CHECK-arm64e-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-arm64e-SAME:     DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-arm64e-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))

  // CHECK-i386: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-i386-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-i386-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-i386-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-i386: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-i386-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-i386-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-i386-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-i386: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-x86_64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-x86_64-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-x86_64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-x86_64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-x86_64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-x86_64-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-x86_64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-x86_64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-x86_64: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-armv7: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-armv7-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-armv7-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-armv7: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-armv7-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-armv7-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-armv7: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-armv7k: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-armv7k-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7k-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-armv7k-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-armv7k: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-armv7k-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7k-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-armv7k-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-armv7k: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-armv7s: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-armv7s-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7s-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-armv7s-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-armv7s: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-armv7s-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-armv7s-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-armv7s-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-armv7s: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-arm64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-arm64-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-arm64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-arm64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-arm64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-arm64-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-arm64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-arm64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-arm64: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-aarch64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-aarch64-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-aarch64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-aarch64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-aarch64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-aarch64-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-aarch64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-aarch64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-aarch64: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-powerpc64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-powerpc64-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-powerpc64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-powerpc64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-powerpc64: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-powerpc64-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-powerpc64-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-powerpc64-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-powerpc64: store i8* %2, i8** %[[ALLOCA]], align

  // CHECK-powerpc64le: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA:[^,]+]],
  // CHECK-powerpc64le-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-powerpc64le-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-powerpc64le-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK-powerpc64le: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-powerpc64le-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-powerpc64le-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-powerpc64le-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK-powerpc64le: store i8* %2, i8** %[[ALLOCA]], align

  use(msg)
}
// CHECK-LABEL: {{^define }}
@main struct Main {
  static func main() async {
    await withGenericArg("hello (asynchronously)")
  }
}
// CHECK: ![[TAU]] = !DILocalVariable(name: "$\CF\84_0_0",
// CHECK: ![[MSG]] = !DILocalVariable(name: "msg", arg: 1,
// CHECK: ![[MSG_R]] = !DILocalVariable(name: "msg", arg: 1,
// CHECK: ![[TAU_R]] = !DILocalVariable(name: "$\CF\84_0_0",

