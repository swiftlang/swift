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

        .686P
        .MODEL flat, C
        .CODE

;;; On entry, [esp + 4] contains a pointer to i386_gprs
;;;
;;; i386_gprs is as follows:
;;;
;;;   Offset    Contents
;;;   ------------------------
;;;   0         eax
;;;   4         ecx
;;;   8         edx
;;;   12        ebx
;;;   16        esp
;;;   20        ebp
;;;   24        esi
;;;   28        edi
;;;   32        eflags
;;;   36        es
;;;   38        cs
;;;   40        ss
;;;   42        ds
;;;   44        fs
;;;   46        gs
;;;   48        eip
;;;   52        validity flags
;;;
_swift_get_cpu_context PROC PUBLIC
        push eax                     ; Stash eax
        mov eax, dword ptr [esp + 8] ; Get the i386_gprs pointer

        ;; General purpose registers
        mov dword ptr [eax + 4], ecx
        mov dword ptr [eax + 8], edx
        mov dword ptr [eax + 12], ebx
        lea edx, [esp + 8]
        mov dword ptr [eax + 16], edx ; esp
        mov dword ptr [eax + 20], ebp
        mov dword ptr [eax + 24], esi
        mov dword ptr [eax + 28], edi

        ;; Now store the original eax
        pop edx
        mov dword ptr [eax], edx

        ;; eflags
        pushfd
        pop edx
        mov dword ptr [eax + 32], edx

        ;; Segment registers
        mov dx, es
        mov word ptr [eax + 36], dx
        mov dx, cs
        mov word ptr [eax + 38], dx
        mov dx, ss
        mov word ptr [eax + 40], dx
        mov dx, ds
        mov word ptr [eax + 42], dx
        mov dx, fs
        mov word ptr [eax + 44], dx
        mov dx, gs
        mov word ptr [eax + 46], dx

        ;; eip (from our return address)
        mov edx, [esp]
        mov dword ptr [eax + 48], edx

        ;; Validity flags (16 valid values)
        mov dword ptr [eax + 52], 0000FFFFh
        ret
_swift_get_cpu_context ENDP

        END
