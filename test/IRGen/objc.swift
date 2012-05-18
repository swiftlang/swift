// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[ID:%_TSs2id]] = type { [[OBJC:%objc_object]]* }

// CHECK:    define [[OBJC]]* @_T4objc5test0FT3argNSs2id_S0_([[OBJC]]* %arg)
// CHECK:      [[RET:%.*]] = alloca [[ID]], align 8
// CHECK-NEXT: [[ARG:%.*]] = alloca [[ID]], align 8
// CHECK-NEXT: [[X:%.*]] = alloca [[ID]], align 8
// CHECK-NEXT: [[Y:%.*]] = alloca [[ID]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[ARG]], i32 0, i32 0
// CHECK-NEXT: store [[OBJC]]* %arg, [[OBJC]]** [[T0]], align 8
func test0(arg : id) -> id {
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[X]], i32 0, i32 0
// CHECK-NEXT: store [[OBJC]]* null, [[OBJC]]** [[T0]], align 8
  var x : id
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[ARG]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = call [[OBJC]]* @objc_retain([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds [[ID]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T4:%.*]] = load [[OBJC]]** [[T3]], align 8
// CHECK-NEXT: store [[OBJC]]* [[T2]], [[OBJC]]** [[T3]], align 8
// CHECK-NEXT: call void @objc_release([[OBJC]]* [[T4]]) nounwind
  x = arg
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds [[ID]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = call [[OBJC]]* @objc_retain([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: store [[OBJC]]* [[T2]], [[OBJC]]** [[T3]], align 8
  var y = x
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds [[ID]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = call [[OBJC]]* @objc_retain([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: store [[OBJC]]* [[T2]], [[OBJC]]** [[T3]], align 8
  return y
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: call void @objc_release([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: call void @objc_release([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[ARG]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: call void @objc_release([[OBJC]]* [[T1]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[ID]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load [[OBJC]]** [[T0]], align 8
// CHECK-NEXT: ret [[OBJC]]* [[T1]]
}
