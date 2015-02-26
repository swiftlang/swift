; REQUIRES: CPU=x86_64
; RUN: llvm-as %s -o %t.bc
; RUN: %swift -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -o %t2.o %t.bc
; RUN: llvm-objdump -m -section __LLVM,__bitcode %t2.o | FileCheck %s
; RUN: llvm-objdump -m -section __LLVM,__cmdline %t2.o | FileCheck -check-prefix=CHECK-CMD %s

target triple = "x86_64-apple-darwin10"

; CHECK: Contents of (__LLVM,__bitcode) section
; CHECK-CMD: Contents of (__LLVM,__cmdline) section
define i32 @f0() nounwind ssp {
       ret i32 0
}
