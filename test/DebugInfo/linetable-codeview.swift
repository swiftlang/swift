// RUN: %swiftc_driver %s -g -debug-info-format=codeview -emit-ir -o - | %FileCheck %s
func markUsed<T>(_ t: T) {}
func arithmetic(_ a: Int64, _ b: Int64) {
  markUsed(a + b)             // line 4
  markUsed(a / b)             // line 5
}
struct SimpleStruct { // NOTE: Classes do not work in Windows yet.
  var myVal1: Float
  var myVal2: Float
  func sum(myArg: Float) {    // line 10
    markUsed(myVal1 + myVal2 + myArg)
  }
}
func myLoop() {
  for index in 0...3 {        // line 15
    markUsed(index)           // line 16
  }
}
func mySwitch(_ a: Int64) {
  switch a {
  case 0:
    markUsed(a-1)             // line 22
  default: do {
      markUsed(a+1)
    }                         // line 25
  }
}

// func arithmetic(_ a: Int64, _ b: Int64)
  // CHECK: define {{.*}} @"$S4main10arithmeticyys5Int64V_ADtF"(i64, i64)
  // CHECK: call { i64, i1 } @llvm.sadd.with.overflow.i64({{.*}}), !dbg ![[ADD:[0-9]+]]
  // NOTE: The division will emit an ``unreachable`` instruction sandwiched
  //       between other instructions for the division. We want to make sure
  //       all instructions from the division have the same debug location and
  //       are contiguous.
  // CHECK: call {{.*}} @"$Ss18_fatalErrorMessage__4file4line5flagss5NeverOs12StaticStringV_A2HSus6UInt32VtF"{{.*}}, !dbg ![[DIV:[0-9]+]]
  // CHECK-NEXT: unreachable, !dbg ![[DIV]]
  // CHECK sdiv i64 %0, %1, !dbg ![[DIV]]
  // CHECK: call void @llvm.trap(), !dbg ![[INLINEDADD:[0-9]+]]
  // CHECK-NEXT: unreachable, !dbg ![[INLINEDADD]]

// func sum(myArg: Float)
  // CHECK: define {{.*}} @"$S4main12SimpleStructV3sum5myArgySf_tF"{{.*}} !dbg ![[SUM:[0-9]+]]
  // NOTE: The point of this test is to trigger IRGenSIL::emitShadowCopy()
  //       and IRGenSIL::emitShadowCopyIfNeeded(). It may be worthwhile to
  //       simplify this testcase.
  // CHECK: store float %0, float* %myArg.addr, {{.*}}, !dbg ![[PROLOGUE:[0-9]+]]
  // CHECK: store float {{.*}}, float* %debug.copy.myVal1._value, {{.*}}, !dbg ![[PROLOGUE]]

// func myLoop() {
  // CHECK: define {{.*}} @"$S4main6myLoopyyF"
  // CHECK: call void @llvm.dbg.declare(metadata i64* %index.addr, {{.*}}), !dbg ![[FORLOOP:[0-9]+]]
  // CHECK: phi i64 [ %{{.[0-9]+}}, %{{.[0-9]+}} ], !dbg ![[FORLOOP]]
  // CHECK: call {{.*}} @"$S4main8markUsedyyxlF"{{.*}}, !dbg ![[FORBODY:[0-9]+]]
  // CHECK: ret void

// func mySwitch(_ a: Int64)
  // CHECK: call { i64, i1 } @llvm.ssub.with.overflow.i64{{.*}}
  // CHECK: br label %[[RETLABEL:[0-9]+]], !dbg ![[CASE:[0-9]+]]
  // CHECK: call { i64, i1 } @llvm.sadd.with.overflow.i64{{.*}}
  // CHECK: br label %[[RETLABEL]], !dbg ![[DEFAULTCLEANUP:[0-9]+]]
  // CHECK: ; <label>:[[RETLABEL]]:
  // CHECK-NEXT: ret void


// CHECK-DAG: ![[ADD]] = !DILocation(line: 4, scope:
// CHECK-DAG: ![[DIV]] = !DILocation(line: 5, scope:
// FIXME: The location of ``@llvm.trap`` should be in Integers.swift.gyb
//        instead of being artificial.
// CHECK-DAG: ![[INLINEDADD]] = !DILocation(line: 0, scope: !{{[0-9]+}}, inlinedAt: ![[ADD]]

// NOTE: These prologue instructions are given artificial line locations for
//       LLDB, but for CodeView they should have the location of the function
//       to keep the linetables contiguous.
// CHECK-DAG: ![[SUM]] = distinct !DISubprogram(name: "sum", linkageName: "$S4main12SimpleStructV3sum5myArgySf_tF"
// CHECK-DAG: ![[PROLOGUE]] = !DILocation(line: 10, scope: ![[SUM]])
// CHECK-DAG: ![[FORLOOP]] = !DILocation(line: 15, scope:
// CHECK-DAG: ![[FORBODY]] = !DILocation(line: 16, scope:
// CHECK-DAG: ![[CASE]] = !DILocation(line: 22, scope:
// CHECK-DAG: ![[DEFAULTCLEANUP]] = !DILocation(line: 25, scope:
