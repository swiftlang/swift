; REQUIRES: CPU=x86_64
; REQUIRES: rdar23493035
; RUN: llvm-as %s -o %t.bc
; RUN: %swift -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -o %t2.o %t.bc -dump-clang-diagnostics 2> %t.diags.txt
; RUN: llvm-objdump -macho -section="__LLVM,__bitcode" %t2.o | %FileCheck %s
; RUN: llvm-objdump -macho -section="__LLVM,__swift_cmdline" %t2.o | %FileCheck -check-prefix=CHECK-CMD %s
; RUN: %FileCheck -check-prefix CHECK-IMPORTER %s < %t.diags.txt

; RUN: %swift -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode-marker  -o %t3.o %t.bc
; RUN: llvm-objdump -macho -section="__LLVM,__bitcode" %t3.o | %FileCheck -check-prefix=MARKER %s
; RUN: llvm-objdump -macho -section="__LLVM,__swift_cmdline" %t3.o | %FileCheck -check-prefix=MARKER-CMD %s

target triple = "x86_64-apple-darwin10"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

; CHECK: Contents of (__LLVM,__bitcode) section
; CHECK-CMD: Contents of (__LLVM,__swift_cmdline) section

; MARKER: Contents of (__LLVM,__bitcode) section
; MARKER-NEXT: 00
; MARKER-CMD: Contents of (__LLVM,__swift_cmdline) section
; MARKER-CMD-NEXT: 00

; CHECK-IMPORTER: clang
; CHECK-IMPORTER: -fembed-bitcode
; CHECK-IMPORTER: -target

define i32 @f0() nounwind ssp {
       ret i32 0
}
