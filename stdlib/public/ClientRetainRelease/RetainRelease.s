
// We currently have an implementation for ARM64, excluding ARM64_32 and
// Android.
#if __arm64__ && __LP64__ && !defined(__ANDROID__)

// The value of 1 strong refcount in the overall refcount field.
#define STRONG_RC_ONE (1 << 33)

.data

// The slowpath mask is in the runtime. Its "address" is the mask, with an
// offset so that it's still correct when the weak reference resolves to zero.
.weak_reference __swift_retainRelease_slowpath_mask_v1

// Grab our own copy of the slowpath mask. This mask is a value which indicates
// when we must call into the runtime slowpath. If the object's refcount field
// has any bits set that are in the mask, then we must take the slow path. The
// offset is the value it will have when the variable isn't present at runtime,
// and needs to be the correct mask for older runtimes.
.align 3
_retainRelease_slowpath_mask:
.quad __swift_retainRelease_slowpath_mask_v1 + 0x8000000000000000


.text

// Save or load all of the registers that we promise to preserve that aren't
// preserved by the standard calling convention. The macro parameter is either
// step or ldp to save or load.
.macro SAVE_LOAD_REGS inst
  \inst x2, x3,  [sp, #(0*8)]
  \inst x4, x5,  [sp, #(2*8)]
  \inst x6, x7,  [sp, #(4*8)]
  \inst x8, x9,  [sp, #(6*8)]
  \inst x10, x11,[sp, #(8*8)]
  \inst x12, x13,[sp, #(10*8)]
  \inst x14, x15,[sp, #(12*8)]
.endmacro

.macro SAVE_REGS
  sub   sp, sp,  #(8*14)
  SAVE_LOAD_REGS stp
.endmacro

.macro LOAD_REGS
  SAVE_LOAD_REGS ldp
.endmacro

.macro CALL_SLOWPATH func
// Push a stack frame.
  stp   fp, lr, [sp, #-16]!
  mov   fp, sp

  SAVE_REGS

// x0 points to the refcount field. Adjust it back to point to the object
// itself.
  sub   x0, x0, #8

// Call the slow path.
  bl \func

  LOAD_REGS

// Pop the stack frame and return.
  mov   sp, fp
  ldp   fp, lr, [sp], #16
  ret
.endmacro


// A note on register usage:
// We use a calling convention that preserves all registers except x0, x1, x16,
// and x17. The fast path can only use those for its scratch registers.

.globl _swift_releaseInlined
_swift_releaseInlined:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lrelease_ret

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x0, x0, #8

// Load the current value in the refcount field.
  ldr   x16, [x0]

// The compare-and-swap goes back to here when it needs to retry.
Lrelease_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set.
  adrp  x17, _retainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, _retainRelease_slowpath_mask@PAGEOFF]
  tst   x16, x17

// Also check if we're releazing with a refcount of 0. That will initiate
// dealloc and requires calling the slow path. We don't try to decrement and
// then call dealloc in that case. We'll just immediately go to the slow path
// and let it take care of the entire operation.
  mov   x17, #STRONG_RC_ONE
  ccmp  x16, x17, #0x8, eq

// If the refcount value matches the slow path mask, or the strong refcount is
// zero, then go to the slow path.
  b.lt  Lslowpath_release

// We're good to proceed with the fast path. Compute the new value of the
// refcount field.
  sub   x17, x16, x17

// Save a copy of the old value so we can determine if the CAS succeeded.
  mov   x1, x16

// Compare and swap the new value into the refcount field. Perform the operation
// with release memory ordering so that dealloc on another thread will see all
// stores performed on this thread prior to calling release.
  casl  x16, x17, [x0]

// The previous value of the refcount field is now in x16. We succeeded if that
// value is the same as the old value we had before. If we failed, retry.
  cmp   x1, x16
  b.ne  Lrelease_retry

// On success, return.
Lrelease_ret:
  ret

Lslowpath_release:
  CALL_SLOWPATH _swift_release


.globl _swift_retainInlined
_swift_retainInlined:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lretain_ret

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x0, x0, #8

// Load the current value of the refcount field.
  ldr   x16, [x0]

Lretain_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set. If it does, go to the slow path.
  adrp  x17, _retainRelease_slowpath_mask@GOTPAGE
  ldr   x17, [x17, _retainRelease_slowpath_mask@GOTPAGEOFF]
  tst   x16, x17
  b.ne  Lslowpath_retain

// Compute a refcount field with the strong refcount incremented.
// TODO: overflow checking
  mov   x17, #STRONG_RC_ONE
  add   x17, x16, x17

// Save the old value so we can check if the CAS succeeded.
  mov   x1, x16

// Compare and swap the new value into the refcount field. Retain can use
// relaxed memory ordering.
  cas   x16, x17, [x0]

// The previous value of the refcount field is now in x16. We succeeded if that
// value is the same as the old value we had before. If we failed, retry.
  cmp   x1, x16
  b.ne  Lretain_retry

// If we succeeded, return. Retain returns the object pointer being retained.
// Readjust x0 to point to the object again instead of the refcount field of the
// object.
  sub   x0, x0, #8

Lretain_ret:
  ret

Lslowpath_retain:
  CALL_SLOWPATH _swift_retain

#else

.globl _placeholderSymbol
.set _placeholderSymbol, 0

#endif
