
// We currently have an implementation for ARM64, excluding ARM64_32 and
// Android.
#if __arm64__ && __LP64__ && !defined(__ANDROID__)

// Use the CAS instructions where available.
#if __ARM_FEATURE_ATOMICS
#define USE_CAS 1
#else
#define USE_CAS 0
#endif

// If CAS is not available, we use load/store exclusive.
#define USE_LDX_STX !USE_CAS

// Use ptrauth instructions where needed.
#if __ARM_FEATURE_PAUTH
#define PTRAUTH 1
#else
#define PTRAUTH 0
#endif

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

// Macro for conditionally emitting instructions. When `condition` is true, the
// rest of the line is emitted. When false, nothing is emitted. More readable
// shorthand for #if blocks when there's only one instruction to conditionalize.
.macro CONDITIONAL condition line:vararg
.ifb \line
.err CONDITIONAL used with no instruction
.endif
.if \condition
\line
.endif
.endmacro


// Save or load all of the registers that we promise to preserve that aren't
// preserved by the standard calling convention. The macro parameter is either
// step or ldp to save or load.
.macro SAVE_LOAD_REGS inst, pushStack
.if \pushStack
  \inst x2, x3,  [sp, #-0x70]!
.else
  \inst x2, x3,  [sp, #0x0]
.endif
  \inst x4, x5,  [sp, #0x10]
  \inst x6, x7,  [sp, #0x20]
  \inst x8, x9,  [sp, #0x30]
  \inst x10, x11,[sp, #0x40]
  \inst x12, x13,[sp, #0x50]
  \inst x14, x15,[sp, #0x60]
.endmacro

.macro SAVE_REGS
  SAVE_LOAD_REGS stp, 1
.endmacro

.macro LOAD_REGS
  SAVE_LOAD_REGS ldp, 0
.endmacro

.macro CALL_SLOWPATH func
// Push a stack frame.
  CONDITIONAL PTRAUTH, \
    pacibsp
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
  CONDITIONAL PTRAUTH, \
    retab
  CONDITIONAL !PTRAUTH, \
    ret
.endmacro


// A note on register usage:
// We use a calling convention that preserves all registers except x0, x1, x16,
// and x17. The fast path can only use those for its scratch registers.

.private_extern _swift_releaseInlined
_swift_releaseInlined:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lrelease_ret

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x0, x0, #8

// Load the current value in the refcount field when using CAS.
  CONDITIONAL USE_CAS, \
    ldr x16, [x0]

// The compare-and-swap goes back to here when it needs to retry.
Lrelease_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set.
  adrp  x17, _retainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, _retainRelease_slowpath_mask@PAGEOFF]

// Load-exclusive of the current value in the refcount field when using LLSC.
// stxr does not update x16 like cas does, so this load must be inside the loop.
// ldxr/stxr are not guaranteed to make forward progress if there are memory
// accesses between them, so we need to do this after getting the mask above.
  CONDITIONAL USE_LDX_STX, \
    ldxr x16, [x0]

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

#if USE_CAS
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
#elif USE_LDX_STX
// Try to store the updated value.
  stlxr  w16, x17, [x0]

// On failure, retry.
  cbnz  w16, Lrelease_retry
#else
#error Either USE_CAS or USE_LDX_STX must be set.
#endif

// On success, return.
Lrelease_ret:
  ret

Lslowpath_release:
  CONDITIONAL USE_LDX_STX, \
    clrex
  CALL_SLOWPATH _swift_release


.private_extern _swift_retainInlined
_swift_retainInlined:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lretain_ret

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x0, x0, #8

// Load the current value of the refcount field when using CAS.
  CONDITIONAL USE_CAS, \
    ldr   x16, [x0]

Lretain_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set.
  adrp  x17, _retainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, _retainRelease_slowpath_mask@PAGEOFF]

// Load-exclusive of the current value in the refcount field when using LLSC.
// stxr does not update x16 like cas does, so this load must be inside the loop.
// ldxr/stxr are not guaranteed to make forward progress if there are memory
// accesses between them, so we need to do this after getting the mask above.
  CONDITIONAL USE_LDX_STX, \
    ldxr x16, [x0]

  tst   x16, x17

  b.ne  Lslowpath_retain

// Compute a refcount field with the strong refcount incremented.
// TODO: overflow checking
  mov   x17, #STRONG_RC_ONE
  add   x17, x16, x17

#if USE_CAS
// Save the old value so we can check if the CAS succeeded.
  mov   x1, x16

// Compare and swap the new value into the refcount field. Retain can use
// relaxed memory ordering.
  cas   x16, x17, [x0]

// The previous value of the refcount field is now in x16. We succeeded if that
// value is the same as the old value we had before. If we failed, retry.
  cmp   x1, x16
  b.ne  Lretain_retry

#elif USE_LDX_STX
// Try to store the updated value.
  stxr  w16, x17, [x0]

// On failure, retry.
  cbnz  w16, Lretain_retry
#else
#error Either USE_CAS or USE_LDX_STX must be set.
#endif

// If we succeeded, return. Retain returns the object pointer being retained.
// Readjust x0 to point to the object again instead of the refcount field of the
// object.
  sub   x0, x0, #8

Lretain_ret:
  ret

Lslowpath_retain:
  CONDITIONAL USE_LDX_STX, \
    clrex
  CALL_SLOWPATH _swift_retain

#else

.private_extern _placeholderSymbol
.set _placeholderSymbol, 0

#endif
