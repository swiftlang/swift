//===--- get-cpu-context.S - Low-level functions to capture registers -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Saves the necessary registers to an appropriate Context structure.
//
//===----------------------------------------------------------------------===//

        // On Apple platforms, we need an extra underscore
#if __APPLE__
  #define FN(x) _##x
#else
  #define FN(x) x
#endif

        .text
        .globl FN(_swift_get_cpu_context)

// .. x86-64 ...................................................................

#if defined(__x86_64__)
        // On entry, rax contains the pointer to the x86_64_gprs
FN(_swift_get_cpu_context):
        movq %rax,           (%rax)
        movq %rdx,          8(%rax)
        movq %rcx,         16(%rax)
        movq %rbx,         24(%rax)
        movq %rsi,         32(%rax)
        movq %rdi,         40(%rax)
        movq %rbp,         48(%rax)
        leaq 8(%rsp),      %rdx
        movq %rdx,         56(%rax)
        movq %r8,          64(%rax)
        movq %r9,          72(%rax)
        movq %r10,         80(%rax)
        movq %r11,         88(%rax)
        movq %r12,         96(%rax)
        movq %r13,         104(%rax)
        movq %r14,         112(%rax)
        movq %r15,         120(%rax)
        pushf
        pop  %rdx
        movq %rdx,         128(%rax)
        movw %cs,          %dx
        movw %dx,          136(%rax)
        movw %fs,          %dx
        movw %dx,          138(%rax)
        movw %gs,          %dx
        movw %dx,          140(%rax)
        movq (%rsp),       %rdx
        movq %rdx,         144(%rax)
        movq $0x1fffff,    152(%rax)
        ret
#endif

// .. i386 .....................................................................

#if defined(__i386__)
        // On entry, 8(%esp) contains the pointer to the i386_gprs
FN(_swift_get_cpu_context):
        push %eax
        movl 8(%esp),  %eax
        movl %ecx,     4(%eax)
        movl %edx,     8(%eax)
        movl %ebx,    12(%eax)
        movl %esi,    16(%eax)
        movl %edi,    20(%eax)
        movl %ebp,    24(%eax)
        leal 8(%esp), %edx
        movl %edx,    28(%eax)
        pushf
        pop  %edx
        movl %edx,    32(%eax)
        movw %ss,     %dx
        movw %dx,     36(%eax)
        movw %cs,     %dx
        movw %dx,     38(%eax)
        movw %ds,     %dx
        movw %dx,     40(%eax)
        movw %es,     %dx
        movw %dx,     42(%eax)
        movw %fs,     %dx
        movw %dx,     44(%eax)
        movw %gs,     %dx
        movw %dx,     46(%eax)
        movl (%esp),  %edx
        movl %edx,    (%eax)
        movl $0xffff, 48(%eax)
        popl %eax
        ret
#endif

// .. ARM64 ....................................................................

#if defined(__aarch64__)
        .p2align 2
        // On entry, x8 contains a pointer to the arm64_gprs
FN(_swift_get_cpu_context):
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
#endif

// .. 32-bit ARM ...............................................................

#if defined(__arm__)
        .p2align 2
        // On entry, r0 contains a pointer to the arm_gprs
FN(_swift_get_cpu_context):
        stm r0, {r0-r14}
        str lr, [r0, #0x3c]
        mov r1, #0xffff
        str r1, [r0, #0x40]
        bx  lr
#endif

