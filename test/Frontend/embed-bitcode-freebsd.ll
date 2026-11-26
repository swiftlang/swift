; REQUIRES: CODEGENERATOR=AArch64
; RUN: llvm-as %s -o %t.bc
; RUN: %swiftc_driver_plain -frontend -target aarch64-unknown-freebsd14 -c -module-name someModule -embed-bitcode -disable-llvm-optzns -o %t2.o %t.bc -dump-clang-diagnostics 2> %t.diags.txt
; RUN: %FileCheck -check-prefix CHECK-IMPORTER %s < %t.diags.txt

; Clang doesn't default to PIC on FreeBSD, so backend jobs need the same -fPIC
; baseline as other Swift compiles.

target triple = "aarch64-unknown-freebsd14"

; CHECK-IMPORTER: clang importer driver args:
; CHECK-IMPORTER-SAME: '-fembed-bitcode'
; CHECK-IMPORTER-SAME: '-fPIC'
; CHECK-IMPORTER-SAME: '-target' 'aarch64-unknown-freebsd14'

define i32 @f0() nounwind {
       ret i32 0
}
