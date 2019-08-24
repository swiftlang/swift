; REQUIRES: CPU=x86_64
; RUN: %swift -target x86_64-apple-darwin10 -S -module-name someModule -o - %s | %FileCheck %s
; RUN: llvm-as %s -o %t.bc
; RUN: %swift -target x86_64-apple-darwin10 -S -module-name someModule -o - %t.bc | %FileCheck -check-prefix=CHECK-BC %s

target triple = "x86_64-apple-darwin10"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

; CHECK: .globl _f0
; CHECK-BC: .globl _f0
define i32 @f0() nounwind ssp {
       ret i32 0
}
