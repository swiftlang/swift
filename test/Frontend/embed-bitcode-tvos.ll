; REQUIRES: CODEGENERATOR=AArch64
; RUN: llvm-as %s -o %t.bc
; RUN: %swift -target arm64-apple-tvos9 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -o %t2.o %t.bc -dump-clang-diagnostics 2> %t.diags.txt
; RUN: llvm-objdump -macho -private-headers %t2.o | %FileCheck %s
; RUN: %FileCheck -check-prefix CHECK-IMPORTER %s < %t.diags.txt

target triple = "arm64-apple-tvos9.0"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"

; CHECK: LC_VERSION_MIN_TVOS

; CHECK-IMPORTER: clang
; CHECK-IMPORTER: -fembed-bitcode
; CHECK-IMPORTER: -target
; CHECK-IMPORTER: arm64-apple-tvos9
; CHECK-IMPORTER-NOT: argument unused

define i32 @f0() nounwind ssp {
       ret i32 0
}
