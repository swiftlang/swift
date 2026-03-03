; ===--- get-cpu-context.asm - Low-level functions to capture registers ---=== ;
;
;  This source file is part of the Swift.org open source project
;
;  Copyright (c) 2025 Apple Inc. and the Swift project authors
;  Licensed under Apache License v2.0 with Runtime Library Exception
;
;  See https://swift.org/LICENSE.txt for license information
;  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
; ===----------------------------------------------------------------------=== ;
;
;  Saves the necessary registers to an appropriate Context structure.
;
; ===----------------------------------------------------------------------=== ;

        .CODE

;; On entry, rcx contains the pointer to the x86_64_gprs
_swift_get_cpu_context PROC PUBLIC
        mov qword ptr [rcx], rax
        mov qword ptr [rcx + 8], rdx
        mov qword ptr [rcx + 16], rcx
        mov qword ptr [rcx + 24], rbx
        mov qword ptr [rcx + 32], rsi
        mov qword ptr [rcx + 40], rdi
        mov qword ptr [rcx + 48], rbp
        lea rdx, [rsp + 8]
        mov qword ptr [rcx + 56], rdx
        mov qword ptr [rcx + 64], r8
        mov qword ptr [rcx + 72], r9
        mov qword ptr [rcx + 80], r10
        mov qword ptr [rcx + 88], r11
        mov qword ptr [rcx + 96], r12
        mov qword ptr [rcx + 104], r13
        mov qword ptr [rcx + 112], r14
        mov qword ptr [rcx + 120], r15
        pushfq
        pop rdx
        mov qword ptr [rcx + 128], rdx
        mov dx, cs
        mov word ptr [rcx + 136], dx
        mov dx, fs
        mov word ptr [rcx + 138], dx
        mov dx, gs
        mov word ptr [rcx + 140], dx
        mov rdx, [rsp]
        mov qword ptr [rcx + 144], rdx
        mov qword ptr [rcx + 152], 1fffffh
        ret
_swift_get_cpu_context ENDP

        END
