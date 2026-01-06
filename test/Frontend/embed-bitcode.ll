; REQUIRES: CPU=x86_64
; RUN: llvm-as %s -o %t.bc

; This test uses '%swiftc_driver_plain -frontend' instead of '%swift' or
; '%target-swift-frontend' because it checks the contents of the command line as
; serialized in the object file.

; RUN: %swiftc_driver_plain -frontend -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -Xllvm -time-passes -o %t2.o %t.bc -dump-clang-diagnostics 2> %t.diags.txt
; RUN: llvm-objdump --macho --section="__LLVM,__bitcode" %t2.o | %FileCheck %s
; RUN: llvm-objdump --macho --section="__LLVM,__swift_cmdline" %t2.o | %FileCheck -check-prefix=CHECK-CMD %s
; RUN: %FileCheck -check-prefix CHECK-COMPILER %s < %t.diags.txt

; RUN: %swiftc_driver_plain -frontend -O -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -Xllvm -time-passes -o %t2-opt.o %t.bc -dump-clang-diagnostics -Xllvm -debug-pass=Structure 2> %t.diags-opt.txt
; RUN: llvm-objdump --macho --section="__LLVM,__bitcode" %t2-opt.o | %FileCheck %s
; RUN: llvm-objdump --macho --section="__LLVM,__swift_cmdline" %t2-opt.o | %FileCheck -check-prefix=CHECK-CMD-OPT %s
; RUN: %FileCheck -check-prefix CHECK-COMPILER-OPT %s < %t.diags-opt.txt

; RUN: %swiftc_driver_plain -frontend -target x86_64-apple-darwin10 -c -module-name someModule -embed-bitcode-marker  -o %t3.o %t.bc
; RUN: llvm-objdump --macho --section="__LLVM,__bitcode" %t3.o | %FileCheck -check-prefix=MARKER %s
; RUN: llvm-objdump --macho --section="__LLVM,__swift_cmdline" %t3.o | %FileCheck -check-prefix=MARKER-CMD %s

target triple = "x86_64-apple-darwin10"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

; CHECK: Contents of (__LLVM,__bitcode) section
; CHECK-CMD: Contents of (__LLVM,__swift_cmdline) section
; -target
; CHECK-CMD: {{^[0-9a-f]+}} 2d 74 61 72 67 65 74 00
; CHECK-CMD-OPT: Contents of (__LLVM,__swift_cmdline) section
; -O -target
; CHECK-CMD-OPT: {{^[0-9a-f]+}} 2d 4f 00 2d 74 61 72 67 65 74 00

; MARKER: Contents of (__LLVM,__bitcode) section
; MARKER-NEXT: 00
; MARKER-CMD: Contents of (__LLVM,__swift_cmdline) section
; MARKER-CMD-NEXT: 00

; CHECK-COMPILER-NOT: argument unused
; CHECK-COMPILER: bin{{/|\\}}clang
; CHECK-COMPILER-SAME: -fembed-bitcode
; CHECK-COMPILER-SAME: -target
; CHECK-COMPILER-NOT: argument unused
; CHECK-COMPILER: Fast Register Allocator

; CHECK-COMPILER-OPT-NOT: argument unused
; CHECK-COMPILER-OPT: bin{{/|\\}}clang
; CHECK-COMPILER-OPT-SAME: -fembed-bitcode
; CHECK-COMPILER-OPT-SAME: -target
; CHECK-COMPILER-OPT-SAME: -Os
; CHECK-COMPILER-OPT-NOT: argument unused
; CHECK-COMPILER-OPT-DAG: Control Flow Optimizer
; CHECK-COMPILER-OPT-DAG: Machine Common Subexpression Elimination

define i32 @f0() nounwind ssp {
       ret i32 0
}
