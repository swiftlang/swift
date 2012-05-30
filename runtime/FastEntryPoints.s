//===--- FastEntryPoints.s - Swift Language Assembly Entry Points ABI -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift Language Assembly Entry Points ABI
//
//===----------------------------------------------------------------------===//

#include "FastEntryPoints.h"

#ifdef __x86_64__
// The custom swift runtime ABI for x86_64 is as follows.
//
// Like normal function calls:
// 1) Arguments to the runtime are in the same registers as normal functions
// 2) Return values are in the same registers as normal functions
// 3) Non-integer registers are NOT preserved: floating point, MMX, SSE, AVX...
// 4) The direction bit is in the forward direction
// 5) %r11 may be trashed by the callee
// 6) Condition flags may be trashed upon completion
//
// Unlike normal function calls:
// 1) The stack need not be aligned
// 2) No stack frame is needed in the caller
// 3) All integer registers other than %r11 and optional return registers are
//    preserved. In other words, if the entry point returns void, then %rax
//    and %rdx are preserved.
//
// This ABI has many advantages. In particular, it helps avoid many register
// spills and even makes unusual tail calls possible:
//
// convertStringToNSSwiftString:
//   mov   %rdi, %rcx     // backup %rdi without spilling
//   mov   $5, %rdi
//   call  swift_rawAlloc
//   lea   NSSwiftStringClass(%rip), %r8
//   mov   %r8,    (%rax) // vtable/isa
//   mov   $1,    8(%rax) // ref count
//   mov   %rcx, 16(%rax) // owner
//   mov   %rsi, 24(%rax) // base  -- NOTE: it didn't need to get spilled
//   mov   %rdx, 32(%rax) // len   -- NOTE: it didn't need to get spilled
//   mov   %rcx, %rdi     // prepare to retain the Swift string
//   jmp   swift_retain   // swift_retain returns void therefore it preserves
//                        // %rax and therefore we can tail call
//                        // NOTE: %rdi and %rax are NOT the same here

.macro BEGIN_FUNC
  .text
  .globl $0
  .align 4
$0:
  .cfi_startproc
.endmacro

.macro END_FUNC
  .cfi_endproc
.endmacro

.macro SaveRegisters
  push  %rbp
  mov   %rsp, %rbp
  // potentially align the stack
  and   $-16, %rsp
  push  %rax
  push  %rcx
  push  %rdx
  push  %rsi
  push  %rdi
  push  %r8
  push  %r9
  push  %r10
.endmacro

.macro RestoreRegisters
  pop   %r10
  pop   %r9
  pop   %r8
  pop   %rdi
  pop   %rsi
  pop   %rdx
  pop   %rcx
  pop   %rax
  // the stack may have been aligned, therefore LEAVE instead of POP %rbp
  leave
.endmacro

// XXX FIXME -- We need to change this to return "void"
BEGIN_FUNC _swift_retain
  test  %rdi, %rdi
  jz    1f
  testb $RC_ATOMIC_BIT, RC_OFFSET(%rdi)
  jnz   3f
  addl  $RC_INTERVAL, RC_OFFSET(%rdi)
  jc    2f
1:
  mov   %rdi, %rax
  ret
2:
  int3
3:
  lock
  addl  $RC_INTERVAL, RC_OFFSET(%rdi)
  jc    2b
  mov   %rdi, %rax
  ret
END_FUNC

BEGIN_FUNC _swift_release
  test  %rdi, %rdi
  jz    1f
  testb $RC_ATOMIC_BIT, RC_OFFSET(%rdi)
  jnz   2f
  subl  $RC_INTERVAL, RC_OFFSET(%rdi)
  jz    4f
  jc    3f
1:
  ret
2:
  // workaround lack of "xsub" instruction via xadd then sub
  movl  $-RC_INTERVAL, %r11d
  lock
  xaddl %r11d, RC_OFFSET(%rdi)
  sub   $RC_INTERVAL, %r11d
  jc    3f
  andl  $RC_MASK, %r11d
  jz    4f
  ret
3:
  int3
4:
  SaveRegisters
  call  __swift_release_slow
  RestoreRegisters
  ret
END_FUNC

BEGIN_FUNC _swift_dealloc
  mov   %gs:SWIFT_TSD_ALLOC_BASE(,%rsi,8), %r11
  mov   %r11, (%rdi)
  mov   %rdi, %gs:SWIFT_TSD_ALLOC_BASE(,%rsi,8)
  ret
END_FUNC

BEGIN_FUNC _swift_rawDealloc
  mov   %gs:SWIFT_TSD_RAW_ALLOC_BASE(,%rsi,8), %r11
  mov   %r11, (%rdi)
  mov   %rdi, %gs:SWIFT_TSD_RAW_ALLOC_BASE(,%rsi,8)
  ret
END_FUNC

.macro ALLOC_FUNC
BEGIN_FUNC $0
1:
  mov   %gs:$1(,%rdi,8), %rax
  test  %rax, %rax
  je    2f
  mov   (%rax), %r11
  mov   %r11, %gs:$1(,%rdi,8)
  ret
2:
  SaveRegisters
.if $2 == 0
  xor   %esi, %esi
.else
  mov   $$$2, %esi
.endif
  call  __swift_refillThreadAllocCache
  RestoreRegisters
.if $2 == SWIFT_TRYALLOC
  cmpq  $$0, %gs:$1(,%rdi,8)
  jnz   1b
  ret
.elseif $2 == SWIFT_TRYRAWALLOC
  cmpq  $$0, %gs:$1(,%rdi,8)
  jnz   1b
  ret
.else
  jmp   1b
.endif
END_FUNC
.endmacro

ALLOC_FUNC _swift_alloc, SWIFT_TSD_ALLOC_BASE, 0
ALLOC_FUNC _swift_tryAlloc, SWIFT_TSD_ALLOC_BASE, SWIFT_TRYALLOC
ALLOC_FUNC _swift_rawAlloc, SWIFT_TSD_RAW_ALLOC_BASE, SWIFT_RAWALLOC
ALLOC_FUNC _swift_tryRawAlloc, SWIFT_TSD_RAW_ALLOC_BASE, SWIFT_TRYRAWALLOC

#endif
