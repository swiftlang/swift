
#ifdef __arm64__

.data
.weak_reference _maskymaskRuntime
.align 3
_maskymask:
.quad _maskymaskRuntime + 0x8000000000000000


.text

.macro SAVE_LOAD_REGS inst
  \inst x0, x1,  [sp, #(0*8)]
  \inst x2, x3,  [sp, #(2*8)]
  \inst x4, x5,  [sp, #(4*8)]
  \inst x6, x7,  [sp, #(6*8)]
  \inst x8, x9,  [sp, #(8*8)]
  \inst x10, x11,[sp, #(10*8)]
  \inst x12, x13,[sp, #(12*8)]
  \inst x14, x15,[sp, #(14*8)]
.endmacro

.macro SAVE_REGS
  sub   sp, sp,  #(8*16)
  SAVE_LOAD_REGS stp
.endmacro

.macro LOAD_REGS
  SAVE_LOAD_REGS ldp
.endmacro

.globl _swift_releaseInlined
_swift_releaseInlined:
  cbz   x0, Lrelease_ret
  add   x0, x0, #8
  ldr   x16, [x0]

Lrelease_retry:
  adrp  x17, _maskymask@PAGE
  ldr   x17, [x17, _maskymask@PAGEOFF]
  tst   x16, x17

  mov   x17, #(1 << 33)
  ccmp  x16, x17, #0x8, eq

  b.lt  Lslowpath_release

  sub   x17, x16, x17
  mov   x1, x16

  casl  x16, x17, [x0]
  cmp   x1, x16
  b.ne  Lrelease_retry

  sub   x0, x0, #8

Lrelease_ret:
  ret

Lslowpath_release:
  stp   fp, lr, [sp, #-16]!
  mov   fp, sp

  SAVE_REGS
  sub   x0, x0, #8
  bl _swift_release
  LOAD_REGS

  mov   sp, fp
  ldp   fp, lr, [sp], #16
  ret


.globl _swift_retainInlined
_swift_retainInlined:
  cbz   x0, Lretain_ret
  add   x0, x0, #8
  ldr   x16, [x0]

Lretain_retry:
  adrp  x17, _maskymask@GOTPAGE
  ldr   x17, [x17, _maskymask@GOTPAGEOFF]
  tst   x16, x17
  b.ne  Lslowpath_retain

  mov   x17, #(1 << 33)
  add   x17, x16, x17
  mov   x1, x16

  casl  x16, x17, [x0]
  cmp   x1, x16
  b.ne  Lretain_retry

  sub   x0, x0, #8

Lretain_ret:
  ret

Lslowpath_retain:
  stp   fp, lr, [sp, #-16]!
  mov   fp, sp

  SAVE_REGS
  sub   x0, x0, #8
  bl   _swift_retain
  LOAD_REGS

  mov   sp, fp
  ldp   fp, lr, [sp], #16
  ret

#else
.globl _placeholderSymbol
.set _placeholderSymbol, 0
#endif
