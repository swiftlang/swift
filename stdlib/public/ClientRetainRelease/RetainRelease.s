
#ifdef __arm64__

.data
.weak_reference _maskymaskRuntime
_maskymask:
.quad _maskymaskRuntime + 0x8000000000000000

.text
.globl _swift_releaseInlined
_swift_releaseInlined:
  cbz   x0, Lret_release
  add   x1, x0, #8
  ldr   x16, [x1]

Lrelease_retry:
  adrp  x17, _maskymask@PAGE
  ldr   x17, [x17, _maskymask@PAGEOFF]
  tst   x16, x17

  mov   x17, #(1 << 33)
  ccmp  x16, x17, #0x8, eq

  b.lt  Lslowpath_release

  sub   x17, x16, x17
  mov   x2, x16

  casl  x16, x17, [x1]
  #deallocating?
  cmp   x2, x16
  b.ne  Lrelease_retry

Lret_release:
  ret

Lslowpath_release:
  stp   fp, lr, [sp, #-16]!
  mov   fp, sp

  sub   sp, sp,  #(8*16)
  stp   x0, x1,  [sp, #(0*8)]
  stp   x2, x3,  [sp, #(2*8)]
  stp   x4, x5,  [sp, #(4*8)]
  stp   x6, x7,  [sp, #(6*8)]
  stp   x8, x9,  [sp, #(8*8)]
  stp   x10, x11,[sp, #(10*8)]
  stp   x12, x13,[sp, #(12*8)]
  stp   x14, x15,[sp, #(14*8)]

  bl _swift_release

  ldp   x0, x1,  [sp, #(0*8)]
  ldp   x2, x3,  [sp, #(2*8)]
  ldp   x4, x5,  [sp, #(4*8)]
  ldp   x6, x7,  [sp, #(6*8)]
  ldp   x8, x9,  [sp, #(8*8)]
  ldp   x10, x11,[sp, #(10*8)]
  ldp   x12, x13,[sp, #(12*8)]
  ldp   x14, x15,[sp, #(14*8)]

  mov   sp, fp
  ldp   fp, lr, [sp], #16
  ret


.globl _swift_retainInlined
_swift_retainInlined:
  ldr   x16, [x0, #8]

  adrp  x17, _maskymask@GOTPAGE
  ldr   x17, [x17, _maskymask@GOTPAGEOFF]
  tst   x16, x17
  b.ne  Lslowpath_retain

  mov   x17, #(1 << 33)
  add   x16, x16, x17
  str   x16, [x0, #8]
  ret

Lslowpath_retain:
  stp   fp, lr, [sp, #-16]!
  mov   fp, sp

  sub   sp, sp,  #(8*16)
  stp   x0, x1,  [sp, #(0*8)]
  stp   x2, x3,  [sp, #(2*8)]
  stp   x4, x5,  [sp, #(4*8)]
  stp   x6, x7,  [sp, #(6*8)]
  stp   x8, x9,  [sp, #(8*8)]
  stp   x10, x11,[sp, #(10*8)]
  stp   x12, x13,[sp, #(12*8)]
  stp   x14, x15,[sp, #(14*8)]

  bl _swift_retain

  ldp   x0, x1,  [sp, #(0*8)]
  ldp   x2, x3,  [sp, #(2*8)]
  ldp   x4, x5,  [sp, #(4*8)]
  ldp   x6, x7,  [sp, #(6*8)]
  ldp   x8, x9,  [sp, #(8*8)]
  ldp   x10, x11,[sp, #(10*8)]
  ldp   x12, x13,[sp, #(12*8)]
  ldp   x14, x15,[sp, #(14*8)]

  mov   sp, fp
  ldp   fp, lr, [sp], #16
  ret

#else
.globl _placeholderSymbol
.set _placeholderSymbol, 0
#endif
