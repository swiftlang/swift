; RUN: %swift-llvm-opt -swift-merge-functions -swiftmergefunc-threshold=4 %s | %FileCheck %s

@g1 = external global i32
@g2 = external global i32
@g3 = external global i32
@g4 = external global i32
@g5 = external global i32

; Test the most trivial example.

; CHECK-LABEL: define i32 @simple_func1(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @simple_func1_merged(i32 %x, i32 %y, i32* @g1)
; CHECK: ret i32 %1
define i32 @simple_func1(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, i32* @g1, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @simple_func2(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @simple_func1_merged(i32 %x, i32 %y, i32* @g2)
; CHECK: ret i32 %1
define i32 @simple_func2(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %l = load i32, i32* @g2, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @simple_func1_merged(i32, i32, i32*)
; CHECK: %l = load i32, i32* %2
; CHECK: ret


; Merge 3 functions with 3 types of differing instructions: load, store and call.

; CHECK-LABEL: define i32 @func1_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3_merged(i32 %x, i32* @g1, i32* @g1, void (i32)* @callee1)
; CHECK: ret i32 %1
define i32 @func1_of_3(i32 %x) {
  %l1 = load i32, i32* @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g1, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, i32 *@g1, align 4
  call void @callee1(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @func2_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3_merged(i32 %x, i32* @g2, i32* @g2, void (i32)* @callee2)
; CHECK: ret i32 %1
define i32 @func2_of_3(i32 %x) {
  %l1 = load i32, i32* @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g2, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, i32 *@g2, align 4
  call void @callee2(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @func3_of_3(i32 %x)
; CHECK: %1 = tail call i32 @func1_of_3_merged(i32 %x, i32* @g3, i32* @g1, void (i32)* @callee3)
; CHECK: ret i32 %1
define i32 @func3_of_3(i32 %x) {
  %l1 = load i32, i32* @g3, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g1, align 4
  %sum2 = add i32 %sum, %l2
  store i32 %sum2, i32 *@g3, align 4
  call void @callee3(i32 %sum2)
  %sum3 = add i32 %sum2, %l2
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @func1_of_3_merged(i32, i32*, i32*, void (i32)*)
; CHECK: %l1 = load i32, i32* %1
; CHECK: %l2 = load i32, i32* %2
; CHECK: store i32 %sum2, i32* %1
; CHECK: call void %3(i32 %sum2)
; CHECK: ret

declare void @callee1(i32 %x)
declare void @callee2(i32 %x)
declare void @callee3(i32 %x)

; Preserve attributes

; CHECK-LABEL: define void @sret_func1(i32* sret %p, i32 %x, i32 %y)
; CHECK: tail call void @sret_func1_merged(i32* sret %p, i32 %x, i32 %y, i32* @g1)
; CHECK: ret void
define void @sret_func1(i32* sret %p, i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %l = load i32, i32* @g1, align 4
  %sum2 = add i32 %sum, %l
  store i32 %sum2, i32* %p
  ret void
}

; CHECK-LABEL: define void @sret_func2(i32* sret %p, i32 %x, i32 %y)
; CHECK: tail call void @sret_func1_merged(i32* sret %p, i32 %x, i32 %y, i32* @g2)
; CHECK: ret void
define void @sret_func2(i32* sret %p, i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %l = load i32, i32* @g2, align 4
  %sum2 = add i32 %sum, %l
  store i32 %sum2, i32* %p
  ret void
}

; CHECK-LABEL: define internal void @sret_func1_merged(i32* sret, i32, i32, i32*)
; CHECK: %l = load i32, i32* %3, align 4
; CHECK: store i32 %sum2, i32* %0
; CHECK: ret


; Don't merge all functions, because we would generate too many parameters.
; Instead merge those functions which match best.

; CHECK-LABEL: define i32 @func1_merged_with3(i32 %x)
; CHECK: %1 = tail call i32 @func1_merged_with3_merged(i32 %x, i32* @g1)
; CHECK: ret i32 %1
define i32 @func1_merged_with3(i32 %x) {
  %l1 = load i32, i32* @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, i32* @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, i32* @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, i32* @g5, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define i32 @func2_merged_with4(i32 %x)
; CHECK: %1 = tail call i32 @func2_merged_with4_merged(i32 %x, i32* @g2)
; CHECK: ret i32 %1
define i32 @func2_merged_with4(i32 %x) {
  %l1 = load i32, i32* @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, i32* @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, i32* @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, i32* @g1, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define i32 @func3_merged_with1(i32 %x)
; CHECK: %1 = tail call i32 @func1_merged_with3_merged(i32 %x, i32* @g2)
; CHECK: ret i32 %1
define i32 @func3_merged_with1(i32 %x) {
  %l1 = load i32, i32* @g2, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g2, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, i32* @g3, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, i32* @g4, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, i32* @g5, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}

; CHECK-LABEL: define internal i32 @func1_merged_with3_merged(i32, i32*)
; CHECK: load i32, i32* %1, align 4
; CHECK: load i32, i32* @g2, align 4
; CHECK: load i32, i32* @g3, align 4
; CHECK: load i32, i32* @g4, align 4
; CHECK: load i32, i32* @g5, align 4
; CHECK: ret i32

; CHECK-LABEL: define i32 @func4_merged_with2(i32 %x) {
; CHECK: %1 = tail call i32 @func2_merged_with4_merged(i32 %x, i32* @g1)
; CHECK: ret i32 %1
define i32 @func4_merged_with2(i32 %x) {
  %l1 = load i32, i32* @g1, align 4
  %sum = add i32 %x, %l1
  %l2 = load i32, i32* @g3, align 4
  %sum2 = add i32 %sum, %l2
  %l3 = load i32, i32* @g4, align 4
  %sum3 = add i32 %sum2, %l2
  %l4 = load i32, i32* @g5, align 4
  %sum4 = add i32 %sum3, %l2
  %l5 = load i32, i32* @g1, align 4
  %sum5 = add i32 %sum4, %l2
  ret i32 %sum5
}


; Test a call chain: caller -> callee1 -> callee2.
; Functions should be merged in bottom-up order: callee2, callee1, caller.
; Also check that the calling convention is preserved.

; CHECK-LABEL: define fastcc i32 @callee1_a(i32 %x, i32 %y)
; CHECK: %1 = tail call fastcc i32 @callee1_a_merged(i32 %x, i32 %y, i32* @g1)
; CHECK: ret i32 %1
define fastcc i32 @callee1_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call i32 @callee2_a(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define fastcc i32 @callee1_b(i32 %x, i32 %y)
; CHECK: %1 = tail call fastcc i32 @callee1_a_merged(i32 %x, i32 %y, i32* @g2)
; CHECK: ret i32 %1
define fastcc i32 @callee1_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call i32 @callee2_b(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define internal fastcc i32 @callee1_a_merged(i32, i32, i32*)
; CHECK: call i32 @callee2_a_merged(i32 %sum2, i32 %1, i32* %2)
; CHECK: ret

; CHECK-NOT: @callee2_a(
define internal i32 @callee2_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = sub i32 %sum, %y
  %l = load i32, i32* @g1, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-NOT: @callee2_b(
define internal i32 @callee2_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = sub i32 %sum, %y
  %l = load i32, i32* @g2, align 4
  %sum3 = add i32 %sum2, %y
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @caller_a(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @caller_a_merged(i32 %x, i32 %y, i32* @g1)
; CHECK: ret i32 %1
define i32 @caller_a(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call fastcc i32 @callee1_a(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define i32 @caller_b(i32 %x, i32 %y)
; CHECK: %1 = tail call i32 @caller_a_merged(i32 %x, i32 %y, i32* @g2)
; CHECK: ret i32 %1
define i32 @caller_b(i32 %x, i32 %y) {
  %sum = add i32 %x, %y
  %sum2 = add i32 %sum, %y
  %c = call fastcc i32 @callee1_b(i32 %sum2, i32 %y)
  %sum3 = add i32 %sum2, %c
  ret i32 %sum3
}

; CHECK-LABEL: define internal i32 @caller_a_merged(i32, i32, i32*)
; CHECK: call fastcc i32 @callee1_a_merged(i32 %sum2, i32 %1, i32* %2)
; CHECK: ret


; Ensure that we do not merge functions that are identical with the
; exception of the order of the incoming blocks to a phi.

; CHECK-LABEL: define linkonce_odr hidden i1 @first(i2)
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

; CHECK-LABEL: define linkonce_odr hidden i1 @second(i2)
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
