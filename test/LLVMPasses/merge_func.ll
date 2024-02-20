; RUN: %swift-llvm-opt -passes='swift-merge-functions' -swiftmergefunc-threshold=4 %s | %FileCheck %s

@g1 = external global i32
@g2 = external global i32
@g3 = external global i32
@g4 = external global i32
@g5 = external global i32

; Test the most trivial example.

; CHECK-LABEL: define i32 @simple_func1(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @simple_func1Tm(i32 %x, i32 %y, ptr @g1)
; CHECK: ret i32 %1
define i32 @simple_func1(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, ptr @g1, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @simple_func2(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @simple_func1Tm(i32 %x, i32 %y, ptr @g2)
; CHECK: ret i32 %1
define i32 @simple_func2(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, ptr @g2, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @simple_func1Tm(i32 %0, i32 %1, ptr %2)
; CHECK: %l = load i32, ptr %2
; CHECK: ret


; Merge 3 functions with 3 types of differing instructions: load, store and call.

; CHECK-LABEL: define i32 @func1_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3Tm(i32 %x, ptr @g1, ptr @g1, ptr @callee1)
; CHECK: ret i32 %1
define i32 @func1_of_3(i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g1, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, ptr @g1, align 4
  call void @callee1(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @func2_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3Tm(i32 %x, ptr @g2, ptr @g2, ptr @callee2)
; CHECK: ret i32 %1
define i32 @func2_of_3(i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, ptr @g2, align 4
  call void @callee2(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @func3_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3Tm(i32 %x, ptr @g3, ptr @g1, ptr @callee3)
; CHECK: ret i32 %1
define i32 @func3_of_3(i32 %x) {
  %l1 = load i32, ptr @g3, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g1, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, ptr @g3, align 4
  call void @callee3(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @func1_of_3Tm(i32 %0, ptr %1, ptr %2, ptr %3)
; CHECK: %l1 = load i32, ptr %1
; CHECK: %l2 = load i32, ptr %2
; CHECK: store i32 %sum2, ptr %1
; CHECK: call void %3(i32 %sum2)
; CHECK: ret

declare void @callee1(i32 %x)
declare void @callee2(i32 %x)
declare void @callee3(i32 %x)

; Preserve attributes

; CHECK-LABEL: define void @sret_func1(ptr sret(i32) %p, i32 %x, i32 %y)
; CHECK: tail call void @sret_func1Tm(ptr sret(i32) %p, i32 %x, i32 %y, ptr @g1)
; CHECK: ret void
define void @sret_func1(ptr sret(i32) %p, i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %l = load i32, ptr @g1, align 4
  %sum2 = add i32 %sum, %l
  store i32 %sum2, ptr %p
  ret void
}

; CHECK-LABEL: define void @sret_func2(ptr sret(i32) %p, i32 %x, i32 %y)
; CHECK: tail call void @sret_func1Tm(ptr sret(i32) %p, i32 %x, i32 %y, ptr @g2)
; CHECK: ret void
define void @sret_func2(ptr sret(i32) %p, i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %l = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l
  store i32 %sum2, ptr %p
  ret void
}

; CHECK-LABEL: define internal void @sret_func1Tm(ptr sret(i32) %0, i32 %1, i32 %2, ptr %3)
; CHECK: %l = load i32, ptr %3, align 4
; CHECK: store i32 %sum2, ptr %0
; CHECK: ret


; Don't merge all functions, because we would generate too many parameters.
; Instead merge those functions which match best.

; CHECK-LABEL: define i32 @func1_merged_with3(i32 %x)
; CHECK: %1 = tail call i32 @func1_merged_with3Tm(i32 %x, ptr @g1)
; CHECK: ret i32 %1
define i32 @func1_merged_with3(i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g5, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define i32 @func2_merged_with4(i32 %x)
; CHECK: %1 = tail call i32 @func2_merged_with4Tm(i32 %x, ptr @g2)
; CHECK: ret i32 %1
define i32 @func2_merged_with4(i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g1, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define i32 @func3_merged_with1(i32 %x)
; CHECK: %1 = tail call i32 @func1_merged_with3Tm(i32 %x, ptr @g2)
; CHECK: ret i32 %1
define i32 @func3_merged_with1(i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g5, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define internal i32 @func1_merged_with3Tm(i32 %0, ptr %1)
; CHECK: load i32, ptr %1, align 4
; CHECK: load i32, ptr @g2, align 4
; CHECK: load i32, ptr @g3, align 4
; CHECK: load i32, ptr @g4, align 4
; CHECK: load i32, ptr @g5, align 4
; CHECK: ret i32

; CHECK-LABEL: define i32 @func4_merged_with2(i32 %x) {
; CHECK: %1 = tail call i32 @func2_merged_with4Tm(i32 %x, ptr @g1)
; CHECK: ret i32 %1
define i32 @func4_merged_with2(i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g1, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}


; The same example as above, but we cannot merge func2 with func4, because
; func4 calls func1 (which is merged with func2 in the first iteration).

declare i32 @get_int(i32 %x)

; CHECK-LABEL: define i32 @Function1_merged_with_3(i32 %x)
; CHECK: %1 = tail call i32 @Function1_merged_with_3Tm(i32 %x, ptr @g1)
; CHECK: ret i32 %1
define i32 @Function1_merged_with_3(i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g5, align 4
  %sum5 = add i32 %sum4, %l2
  %c = call fastcc i32 @get_int(i32 %sum5)
  ret i32 %c
}

; CHECK-LABEL: define i32 @Function2_not_merged(i32 %x)
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: %c = call fastcc i32 @get_int
; CHECK: ret i32 %c
define i32 @Function2_not_merged(i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g1, align 4
  %sum5 = add i32 %sum4, %l2
  %c = call fastcc i32 @get_int(i32 %sum5)
  ret i32 %c
}

; CHECK-LABEL: define i32 @Function3_merged_with_1(i32 %x)
; CHECK: %1 = tail call i32 @Function1_merged_with_3Tm(i32 %x, ptr @g2)
; CHECK: ret i32 %1
define i32 @Function3_merged_with_1(i32 %x) {
  %l1 = load i32, ptr @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g5, align 4
  %sum5 = add i32 %sum4, %l2
  %c = call fastcc i32 @get_int(i32 %sum5)
  ret i32 %c
}

; CHECK-LABEL: define internal i32 @Function1_merged_with_3Tm(i32 %0, ptr %1)
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: %c = call fastcc i32 @get_int
; CHECK: ret i32 %c

; CHECK-LABEL: define i32 @Function4_not_merged(i32 %x) {
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: load
; CHECK: %1 = call fastcc i32 @Function1_merged_with_3Tm(i32 %sum5, ptr @g1)
; CHECK: ret i32 %1
define i32 @Function4_not_merged(i32 %x) {
  %l1 = load i32, ptr @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, ptr @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, ptr @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, ptr @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, ptr @g1, align 4
  %sum5 = add i32 %sum4, %l2
  %c = call fastcc i32 @Function1_merged_with_3(i32 %sum5)
  ret i32 %c
}


; Test a call chain: caller -> callee1 -> callee2.
; Functions should be merged in bottom-up order: callee2, callee1, caller.
; Also check that the calling convention is preserved.

; CHECK-LABEL: define fastcc i32 @callee1_a(i32 %x, i32 %y)
; CHECK: %1 = tail call fastcc i32 @callee1_aTm(i32 %x, i32 %y, ptr @g1)
; CHECK: ret i32 %1
define fastcc i32 @callee1_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call i32 @callee2_a(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define fastcc i32 @callee1_b(i32 %x, i32 %y)
; CHECK: %1 = tail call fastcc i32 @callee1_aTm(i32 %x, i32 %y, ptr @g2)
; CHECK: ret i32 %1
define fastcc i32 @callee1_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call i32 @callee2_b(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define internal fastcc i32 @callee1_aTm(i32 %0, i32 %1, ptr %2)
; CHECK: call i32 @callee2_aTm(i32 %sum2, i32 %1, ptr %2)
; CHECK: ret

; CHECK-NOT: @callee2_a(
define internal i32 @callee2_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = sub i32 %sum, %y
  %l = load i32, ptr @g1, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-NOT: @callee2_b(
define internal i32 @callee2_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = sub i32 %sum, %y
  %l = load i32, ptr @g2, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @caller_a(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @caller_aTm(i32 %x, i32 %y, ptr @g1)
; CHECK: ret i32 %1
define i32 @caller_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call fastcc i32 @callee1_a(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @caller_b(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @caller_aTm(i32 %x, i32 %y, ptr @g2)
; CHECK: ret i32 %1
define i32 @caller_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call fastcc i32 @callee1_b(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @caller_aTm(i32 %0, i32 %1, ptr %2)
; CHECK: call fastcc i32 @callee1_aTm(i32 %sum2, i32 %1, ptr %2)
; CHECK: ret


; Ensure that we do not merge functions that are identical with the
; exception of the order of the incoming blocks to a phi.

; CHECK-LABEL: define linkonce_odr hidden i1 @first(i2 %0)
define linkonce_odr hidden i1 @first(i2) {
entry:
; CHECK: switch i2
  switch i2 %0, label %default [
    i2 0, label %L1
    i2 1, label %L2
    i2 -2, label %L3
  ]
default:
  unreachable
L1:
  br label %done
L2:
  br label %done
L3:
  br label %done
done:
  %result = phi i1 [ true, %L1 ], [ false, %L2 ], [ false, %L3 ]
; CHECK: ret i1
  ret i1 %result
}

; CHECK-LABEL: define linkonce_odr hidden i1 @second(i2 %0)
define linkonce_odr hidden i1 @second(i2) {
entry:
; CHECK: switch i2
  switch i2 %0, label %default [
    i2 0, label %L1
    i2 1, label %L2
    i2 -2, label %L3
  ]
default:
  unreachable
L1:
  br label %done
L2:
  br label %done
L3:
  br label %done
done:
  %result = phi i1 [ true, %L3 ], [ false, %L2 ], [ false, %L1 ]
; CHECK: ret i1
  ret i1 %result
}

; Check self recursive functions

; CHECK-LABEL: define internal void @recursive1(i32 %x, i32 %y)
; CHECK: tail call void @recursive1Tm(i32 %x, i32 %y, ptr @g1, ptr @recursive1)
; CHECK: ret void
define internal void @recursive1(i32 %x, i32 %y) {
  br i1 undef, label %bb1, label %bb2

bb1:
  %l = load i32, ptr @g1, align 4
  call void @recursive1(i32 %x, i32 %y)
  br label %bb2

bb2:
  ret void
}

; CHECK-LABEL: define internal void @recursive2(i32 %x, i32 %y)
; CHECK: tail call void @recursive1Tm(i32 %x, i32 %y, ptr @g2, ptr @recursive2)
; CHECK: ret void
define internal void @recursive2(i32 %x, i32 %y) {
  br i1 undef, label %bb1, label %bb2

bb1:
  %l = load i32, ptr @g2, align 4
  call void @recursive2(i32 %x, i32 %y)
  br label %bb2

bb2:
  ret void
}
; CHECK-LABEL: define internal void @recursive1Tm(i32 %0, i32 %1, ptr %2, ptr %3)
; CHECK: load i32, ptr %2
; CHECK: call void %3(i32 %0, i32 %1)
; CHECK: ret void


; CHECK-LABEL: define internal void @another_recursive_func(i32 %x)
; CHECK: tail call void @another_recursive_funcTm(i32 %x, ptr @g1, ptr @another_recursive_func)
; CHECK: ret void
define internal void @another_recursive_func(i32 %x) {
  br i1 undef, label %bb1, label %bb2

bb1:
  store i32 %x, ptr @g1, align 4
  call void @another_recursive_func(i32 %x)
  br label %bb2

bb2:
  ret void
}
; CHECK-NOT: @not_really_recursive(

; CHECK-LABEL: define internal void @another_recursive_funcTm(i32 %0, ptr %1, ptr %2)
; CHECK: store i32 %0, ptr %1
; CHECK: call void %2(i32 %0)
; CHECK: ret void
define internal void @not_really_recursive(i32 %x) {
  br i1 undef, label %bb1, label %bb2

bb1:
  store i32 %x, ptr @g2, align 4
  call void @callee1(i32 %x)
  br label %bb2

bb2:
  ret void
}
; CHECK-NOT: @not_really_recursive(

; CHECK-LABEL: define void @call_recursive_funcs(i32 %x)
; CHECK: call void @recursive1(i32 %x, i32 %x)
; CHECK: call void @recursive2(i32 %x, i32 %x)
; CHECK: call void @another_recursive_func(i32 %x)
; CHECK: call void @another_recursive_funcTm(i32 %x, ptr @g2, ptr @callee1)
; CHECK: ret void
define void @call_recursive_funcs(i32 %x) {
  call void @recursive1(i32 %x, i32 %x)
  call void @recursive2(i32 %x, i32 %x)
  call void @another_recursive_func(i32 %x)
  call void @not_really_recursive(i32 %x)
  ret void
}

; Ensure that we do not merge functions which make use of distinct dtrace
; probes. Each call to a dtrace probe must resolve to a unique patchpoint.

declare void @"__dtrace_probe$Apple$Probe1$v1$696e74"(i32) local_unnamed_addr

; CHECK-LABEL: define i32 @use_dtrace_probe1
; CHECK: call void @"__dtrace_probe$Apple$Probe1$v1$696e74"
define i32 @use_dtrace_probe1(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, ptr @g1, align 4
  %sum3 = add i32 %sum2, %y
  tail call void @"__dtrace_probe$Apple$Probe1$v1$696e74"(i32 undef)
  ret i32 %sum3
}

declare void @"__dtrace_probe$Apple$Probe2$v1$696e74"(i32) local_unnamed_addr

; CHECK-LABEL: define i32 @use_dtrace_probe2
; CHECK: call void @"__dtrace_probe$Apple$Probe2$v1$696e74"
define i32 @use_dtrace_probe2(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, ptr @g2, align 4
  %sum3 = add i32 %sum2, %y
  tail call void @"__dtrace_probe$Apple$Probe2$v1$696e74"(i32 undef)
  ret i32 %sum3
}
