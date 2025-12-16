; ===--- get-cpu-context-aarch64.asm - Low-level register capture ---------=== ;
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

        AREA |.text|, CODE, READONLY, ARM64
        EXPORT _swift_get_cpu_context

;; On entry, rax contains the pointer to the arm64_gprs
_swift_get_cpu_context PROC PUBLIC
        stp  x0,  x1, [x8, #0x00]
        stp  x2,  x3, [x8, #0x10]
        stp  x4,  x5, [x8, #0x20]
        stp  x6,  x7, [x8, #0x30]
        stp  x8,  x9, [x8, #0x40]
        stp x10, x11, [x8, #0x50]
        stp x12, x13, [x8, #0x60]
        stp x14, x15, [x8, #0x70]
        stp x16, x17, [x8, #0x80]
        stp x18, x19, [x8, #0x90]
        stp x20, x21, [x8, #0xa0]
        stp x22, x23, [x8, #0xb0]
        stp x24, x25, [x8, #0xc0]
        stp x26, x27, [x8, #0xd0]
        stp x28, x29, [x8, #0xe0]
        mov  x1, sp
        stp x30, x1,  [x8, #0xf0]
        str x30,      [x8, #0x100]
        mov  x1, #0x1ffffffff
        str  x1,      [x8, #0x108]
        ret
_swift_get_cpu_context ENDP

        END
