// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

protocol Set {
  func empty() -> Bool
  func size() -> Int
  func contains(i : Int) -> Bool
}
protocol Displayable {
  func display()
}

// CHECK: [[SSET:%_T19ProtocolComposition12SingletonSet]] = type { [[INT:%.*]] }
// CHECK: [[INT:%_TSs5Int64]] = type { i64 }
// CHECK: [[BOOL:%_TSs4Bool]] = type { i1 }
// CHECK: [[COMP0:%"_T19ProtocolComposition11Displayable\+_T19ProtocolComposition3Set"]] = type { i8**, i8**, [[BUFFER:.*]] }
// CHECK: [[OPAQUE:%swift.opaque]] = type opaque

// CHECK: [[WTBL_SSET_DISPLAYABLE:@witness_table.*]] = internal constant [14 x i8*]
// CHECK: [[WTBL_SSET_SET:@witness_table.*]] = internal constant [16 x i8*]

struct SingletonSet {
  var value : Int
}
extension SingletonSet : Set {
  func empty() -> Bool { return false }
  func size() -> Int { return 1 }
  func contains(i : Int) -> Bool { return i == value }
}
extension SingletonSet : Displayable {
  func display() { print("\(value)") }
}

func test0() -> protocol<Set,Displayable> {
  var x = SingletonSet(5)
  return x
}
// CHECK:    define void @_T19ProtocolComposition5test0FT_CPS_3SetPS_11Displayable_([[COMP0]]* noalias sret) {
//   Initialize 'x'.
// CHECK:      [[X:%.*]] = alloca [[SSET]], align 8
// CHECK-NEXT: [[T0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 5)
// CHECK-NEXT: [[T1:%.*]] = call i64 @_TN19ProtocolComposition12SingletonSet11constructorFT5valueNSs5Int64_S0_(i64 [[T0]])
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds [[SSET]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds [[INT]]* [[T2]], i32 0, i32 0
// CHECK-NEXT: store i64 [[T1]], i64* [[T3]], align 8
//   Copy 'x' into the return slot.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[RET:%0]], i32 0, i32 2
// CHECK-NEXT: [[DEST:%.*]] = bitcast [[BUFFER]]* [[T0]] to [[SSET]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SSET]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[V:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SSET]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V]], i64* [[T1]], align 8
//   Set the witness tables.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: store i8** getelementptr inbounds ([14 x i8*]* [[WTBL_SSET_DISPLAYABLE]], i64 0, i64 0), i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[RET]], i32 0, i32 1
// CHECK-NEXT: store i8** getelementptr inbounds ([16 x i8*]* [[WTBL_SSET_SET]], i64 0, i64 0), i8*** [[T0]], align 8
//   All done.
// CHECK-NEXT: ret void

func test1(p : protocol<Set,Displayable>) {
  if (!p.empty()) {
    p.display()
  }
}
// CHECK:    define void @_T19ProtocolComposition5test1FT1pCPS_3SetPS_11Displayable__T_([[COMP0]]* [[P:%p]]) {
//   Initialize 'x'.
// CHECK:      [[TEMP1:%.*]] = alloca [[BUFFER]], align 8
// CHECK-NEXT: [[TEMP2:%.*]] = alloca [[BUFFER]], align 8
//   Copy the value in P into a temporary.  Note that we're using the Displayable protocol.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 1
// CHECK-NEXT: [[W:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 2
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8** [[W]], i32 1
// CHECK-NEXT: [[T2:%.*]] = load i8** [[T1]], align 8
// CHECK-NEXT: [[COPY:%.*]] = bitcast i8* [[T2]] to [[OPAQUE]]* ([[BUFFER]]*, [[BUFFER]]*, i8**)*
// CHECK-NEXT: [[V:%.*]] = call [[OPAQUE]]* [[COPY]]([[BUFFER]]* noalias [[TEMP1]], [[BUFFER]]* [[T0]], i8** [[W]]) nounwind
//   Call empty() on the temporary.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** [[W]], i32 13
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[EMPTY:%.*]] = bitcast i8* [[T1]] to i1 ([[OPAQUE]]*)*
// CHECK-NEXT: [[T0:%.*]] = call i1 [[EMPTY]]([[OPAQUE]]* [[V]])
//   Call !
// CHECK-NEXT: [[T1:%.*]] = call i1 @_TSsop1nFT1aNSs4Bool_S_(i1 [[T0]])
//   Convert to logic value.  It's silly that we special-case this but
//   still leave in the materialization.
// CHECK-NEXT: getelementptr
// CHECK-NEXT: store
// CHECK-NEXT: getelementptr
// CHECK-NEXT: [[C:%.*]] = load
// CHECK-NEXT: [[T0:%.*]] = load i8** [[W]], align 8
//   Destroy the temporary.
// CHECK-NEXT: [[DESTROY:%.*]] = bitcast i8* [[T0]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[DESTROY]]([[BUFFER]]* [[TEMP1]], i8** [[W]]) nounwind
// CHECK-NEXT: br i1 [[C]], label %[[TRUE:[a-z.]+]], label %[[FALSE:[a-z.]+]]
// CHECK:    [[TRUE]]:
//   Copy the value in P into a temporary.  Note that we're using the Set protocol.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 0
// CHECK-NEXT: [[W:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 2
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8** [[W]], i32 1
// CHECK-NEXT: [[T2:%.*]] = load i8** [[T1]], align 8
// CHECK-NEXT: [[COPY:%.*]] = bitcast i8* [[T2]] to [[OPAQUE]]* ([[BUFFER]]*, [[BUFFER]]*, i8**)*
// CHECK-NEXT: [[V:%.*]] = call [[OPAQUE]]* [[COPY]]([[BUFFER]]* noalias [[TEMP2]], [[BUFFER]]* [[T0]], i8** [[W]]) nounwind
//   Call display() on the temporary.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8** [[W]], i32 13
// CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]], align 8
// CHECK-NEXT: [[DISPLAY:%.*]] = bitcast i8* [[T1]] to void ([[OPAQUE]]*)*
// CHECK-NEXT: call void [[DISPLAY]]([[OPAQUE]]* [[V]])
//   Destroy the temporary.
// CHECK-NEXT: [[T0:%.*]] = load i8** [[W]], align 8
// CHECK-NEXT: [[DESTROY:%.*]] = bitcast i8* [[T0]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[DESTROY]]([[BUFFER]]* [[TEMP2]], i8** [[W]]) nounwind
// CHECK-NEXT: br label %[[FALSE]]
//   Destroy 'P'.
// CHECK:   [[FALSE]]:
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 0
// CHECK-NEXT: [[W:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[COMP0]]* [[P]], i32 0, i32 2
// CHECK-NEXT: [[T1:%.*]] = load i8** [[W]], align 8
// CHECK-NEXT: [[DESTROY:%.*]] = bitcast i8* [[T1]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[DESTROY]]([[BUFFER]]* [[T0]], i8** [[W]]) nounwind
//   Done.
// CHECK-NEXT: ret void
