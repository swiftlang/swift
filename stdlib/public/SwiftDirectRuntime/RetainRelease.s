
// We currently have an implementation for ARM64 Mach-O.
#if __arm64__ && __LP64__ && defined(__APPLE__) && defined(__MACH__)

#include "swift/ABI/System.h"

// Use the CAS instructions where available.
#if __ARM_FEATURE_ATOMICS
#define USE_CAS 1
#else
#define USE_CAS 0
#endif

// If CAS is not available, we use load/store exclusive.
#define USE_LDX_STX !USE_CAS

// Use ptrauth instructions where needed.
#if __has_feature(ptrauth_calls)
#define PTRAUTH 1
#else
#define PTRAUTH 0
#endif

// The value of 1 strong refcount in the overall refcount field.
#define STRONG_RC_ONE (1 << 33)

// The mask to apply to BridgeObject values to extract the pointer they contain.
#define BRIDGEOBJECT_POINTER_BITS (~SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK)


.subsections_via_symbols

.data

// The slowpath mask is in the runtime. Its "address" is the mask, with an
// offset so that it's still correct when the weak reference resolves to zero.
.weak_reference __swift_retainRelease_slowpath_mask_v1

// preservemost retain/release entrypoints in the runtime
.weak_reference _swift_retain_preservemost
.weak_reference _swift_release_preservemost


// Grab our own copy of the slowpath mask. This mask is a value which indicates
// when we must call into the runtime slowpath. If the object's refcount field
// has any bits set that are in the mask, then we must take the slow path. The
// offset is the value it will have when the variable isn't present at runtime,
// and needs to be the correct mask for older runtimes.
//
// This variable goes into a special section so it can be located and runtime
// to override the value. Instrumentation can set it to all 1s to ensure the
// slow path is always used.
.section __DATA,__swift5_rr_mask
.align 3
LretainRelease_slowpath_mask:
.quad __swift_retainRelease_slowpath_mask_v1 + 0x8000000000000000


.text
.align 2

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


// Helper macros for conditionally supporting ptrauth.
.macro maybe_pacibsp
.if PTRAUTH
pacibsp
.endif
.endmacro

.macro ret_maybe_ab
.if PTRAUTH
retab
.else
ret
.endif
.endmacro


// NOTE: we're using the preserve_most calling convention, so x9-15 are off
// limits, in addition to the usual x19 and up. Any calls to functions that use
// the standard calling convention need to save/restore x9-x15.

.private_extern _swift_bridgeObjectReleaseDirect
#if SWIFT_OBJC_INTEROP
_swift_bridgeObjectReleaseDirect:
  tbz  x0, #63, LbridgeObjectReleaseNotTagged
  ret
LbridgeObjectReleaseNotTagged:
  tbnz  x0, #62, LbridgeObjectReleaseDirectObjC
  and   x0, x0, 0x0ffffffffffffff8

#else
_swift_bridgeObjectReleaseDirect:
  and   x0, x0, 0x0ffffffffffffff8
#endif

.alt_entry _swift_releaseDirect
.private_extern _swift_releaseDirect
_swift_releaseDirect:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lrelease_ret

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x1, x0, #8

// Load the current value in the refcount field when using CAS.
  CONDITIONAL USE_CAS, \
    ldr x16, [x1]

// The compare-and-swap goes back to here when it needs to retry.
Lrelease_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set.
  adrp  x17, LretainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, LretainRelease_slowpath_mask@PAGEOFF]

// Load-exclusive of the current value in the refcount field when using LLSC.
// stxr does not update x16 like cas does, so this load must be inside the loop.
// ldxr/stxr are not guaranteed to make forward progress if there are memory
// accesses between them, so we need to do this after getting the mask above.
  CONDITIONAL USE_LDX_STX, \
    ldxr x16, [x1]

  tst   x16, x17

// Also check if we're releasing with a refcount of 0. That will initiate
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
  mov   x2, x16

// Compare and swap the new value into the refcount field. Perform the operation
// with release memory ordering so that dealloc on another thread will see all
// stores performed on this thread prior to calling release.
  casl  x16, x17, [x1]

// The previous value of the refcount field is now in x16. We succeeded if that
// value is the same as the old value we had before. If we failed, retry.
  cmp   x2, x16
  b.ne  Lrelease_retry
#elif USE_LDX_STX
// Try to store the updated value.
  stlxr  w16, x17, [x1]

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
// If the weak preservemost symbol is NULL, call our helper. Otherwise call the
// runtime directly.
  adrp x17, _swift_release_preservemost@GOTPAGE
  ldr  x17, [x17, _swift_release_preservemost@GOTPAGEOFF]
  cbz x17, Lcall_swift_release
  b _swift_release_preservemost

// Save/restore the preservemost registers and call swift_retain.
Lcall_swift_release:
  maybe_pacibsp
  str   x9, [sp, #-0x50]!
  stp   x10, x11, [sp, #0x10]
  stp   x12, x13, [sp, #0x20]
  stp   x14, x15, [sp, #0x30]
  stp   fp, lr, [sp, #0x40];
  add   fp, sp, #0x40

  // Clear the unused bits from the pointer
  and   x0, x0, #BRIDGEOBJECT_POINTER_BITS
  bl    _swift_release

  ldp   fp, lr, [sp, #0x40]
  ldp   x14, x15, [sp, #0x30]
  ldp   x12, x13, [sp, #0x20]
  ldp   x10, x11, [sp, #0x10]
  ldr   x9, [sp], #0x50
  ret_maybe_ab

LbridgeObjectReleaseDirectObjC:
  maybe_pacibsp
  stp   x0, x9, [sp, #-0x50]!
  stp   x10, x11, [sp, #0x10]
  stp   x12, x13, [sp, #0x20]
  stp   x14, x15, [sp, #0x30]
  stp   fp, lr, [sp, #0x40];
  add   fp, sp, #0x40

  // Clear the unused bits from the pointer
  and   x0, x0, #BRIDGEOBJECT_POINTER_BITS
  bl    _objc_release

  ldp   fp, lr, [sp, #0x40]
  ldp   x14, x15, [sp, #0x30]
  ldp   x12, x13, [sp, #0x20]
  ldp   x10, x11, [sp, #0x10]
  ldp   x0, x9, [sp], #0x50
LbridgeObjectReleaseObjCRet:
  ret_maybe_ab


.private_extern _swift_bridgeObjectRetainDirect
#if SWIFT_OBJC_INTEROP
_swift_bridgeObjectRetainDirect:
  tbz  x0, #63, LbridgeObjectRetainNotTagged
  ret
LbridgeObjectRetainNotTagged:
  tbnz  x0, #62, Lswift_bridgeObjectRetainDirectObjC

.alt_entry _swift_retainDirect
#else
.set _swift_bridgeObjectRetainDirect, _swift_retainDirect
#endif

.private_extern _swift_retainDirect
_swift_retainDirect:
// RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lretain_ret

// Mask off spare bits that may have come in from bridgeObjectRetain. Keep the
// original value in x0 since we have to return it.
  and   x1, x0, 0xffffffffffffff8

// We'll operate on the address of the refcount field, which is 8 bytes into
// the object.
  add   x1, x1, #8

// Load the current value of the refcount field when using CAS.
  CONDITIONAL USE_CAS, \
    ldr   x16, [x1]

Lretain_retry:
// Get the slow path mask and see if the refcount field has any of those bits
// set.
  adrp  x17, LretainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, LretainRelease_slowpath_mask@PAGEOFF]

// Load-exclusive of the current value in the refcount field when using LLSC.
// stxr does not update x16 like cas does, so this load must be inside the loop.
// ldxr/stxr are not guaranteed to make forward progress if there are memory
// accesses between them, so we need to do this after getting the mask above.
  CONDITIONAL USE_LDX_STX, \
    ldxr x16, [x1]

// Compute a refcount field with the strong refcount incremented.
  mov   x3, #STRONG_RC_ONE
  add   x3, x16, x3

  // Test the incremented value against the slowpath mask. This checks for both
  // the side table case and the overflow case, as the overflow sets the high
  // bit. This can't have a false negative, as clearing the bit with an overflow
  // would require the refcount field to contain a side table pointer with a top
  // set to 0x7fff, which wouldn't be a valid pointer.
  tst   x3, x17
  b.ne  Lslowpath_retain

#if USE_CAS
// Save the old value so we can check if the CAS succeeded.
  mov   x2, x16

// Compare and swap the new value into the refcount field. Retain can use
// relaxed memory ordering.
  cas   x16, x3, [x1]

// The previous value of the refcount field is now in x16. We succeeded if that
// value is the same as the old value we had before. If we failed, retry.
  cmp   x2, x16
  b.ne  Lretain_retry

#elif USE_LDX_STX
// Try to store the updated value.
  stxr  w16, x3, [x1]

// On failure, retry.
  cbnz  w16, Lretain_retry
#else
#error Either USE_CAS or USE_LDX_STX must be set.
#endif

// If we succeeded, return. Retain returns the object pointer being retained,
// which is still in x0 at this point.

Lretain_ret:
  ret

Lslowpath_retain:
  CONDITIONAL USE_LDX_STX, \
    clrex
// If the weak preservemost symbol is NULL, call our helper. Otherwise call the
// runtime directly.
  adrp x17, _swift_retain_preservemost@GOTPAGE
  ldr  x17, [x17, _swift_retain_preservemost@GOTPAGEOFF]
  cbz x17, Lcall_swift_retain
  b _swift_retain_preservemost

// Save/restore the preservemost registers and call swift_retain.
Lcall_swift_retain:
  maybe_pacibsp
  stp   x0, x9, [sp, #-0x50]!
  stp   x10, x11, [sp, #0x10]
  stp   x12, x13, [sp, #0x20]
  stp   x14, x15, [sp, #0x30]
  stp   fp, lr, [sp, #0x40];
  add   fp, sp, #0x40

  // Clear the unused bits from the pointer
  and   x0, x0, #BRIDGEOBJECT_POINTER_BITS
  bl    _swift_retain

  ldp   fp, lr, [sp, #0x40]
  ldp   x14, x15, [sp, #0x30]
  ldp   x12, x13, [sp, #0x20]
  ldp   x10, x11, [sp, #0x10]
  ldp   x0, x9, [sp], #0x50
  ret_maybe_ab

Lswift_bridgeObjectRetainDirectObjC:
  maybe_pacibsp
  stp   x0, x9, [sp, #-0x50]!
  stp   x10, x11, [sp, #0x10]
  stp   x12, x13, [sp, #0x20]
  stp   x14, x15, [sp, #0x30]
  stp   fp, lr, [sp, #0x40];
  add   fp, sp, #0x40

  // Clear the unused bits from the pointer
  and   x0, x0, #BRIDGEOBJECT_POINTER_BITS
  bl    _objc_retain

  ldp   fp, lr, [sp, #0x40]
  ldp   x14, x15, [sp, #0x30]
  ldp   x12, x13, [sp, #0x20]
  ldp   x10, x11, [sp, #0x10]
  ldp   x0, x9, [sp], #0x50
  ret_maybe_ab

#else

.private_extern _placeholderSymbol
.set _placeholderSymbol, 0

#endif
