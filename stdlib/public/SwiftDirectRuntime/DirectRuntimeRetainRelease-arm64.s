; DirectRuntimeRetainRelease-arm64.s - ARM64 asm for direct retain/release
;
; This file is read by the Swift compiler at compile time and emitted as
; module-level inline assembly into each .o file. The functions use
; .weak_definition so the linker coalesces duplicates.
;
; The compiler prepends .set directives to parameterize this assembly:
;
;   USE_CAS                      - 1 if the target has LSE atomics
;   OBJC_INTEROP                 - 1 if ObjC interop is enabled
;   TARGET_HAS_PRESERVEMOST      - 1 if the deployment target guarantees
;                                  the preservemost retain/release entrypoints
;   NEED_RETAIN                  - 1 if any retain functions are referenced
;   NEED_RELEASE                 - 1 if any release functions are referenced
;   STRONG_RC_ONE                - value of one strong refcount in the
;                                  refcount field
;   BRIDGEOBJECT_POINTER_BITS    - mask to extract the pointer from a
;                                  BridgeObject value

.subsections_via_symbols

; preservemost retain/release entrypoints in the runtime. When the deployment
; target doesn't guarantee they exist, use weak references.
.if TARGET_HAS_PRESERVEMOST == 0
  .weak_reference _swift_retain_preservemost
  .weak_reference _swift_release_preservemost
.endif

.text
.align 2

; Enable target-specific instructions. Module-level inline asm doesn't inherit
; target features from the LLVM module.
.if USE_CAS
  .arch_extension lse
.endif


; NOTE: we're using the preserve_most calling convention, so x9-15 are off
; limits, in addition to the usual x19 and up. Any calls to functions that use
; the standard calling convention need to save/restore x9-x15.


; =============================================================================
; Release
; =============================================================================

; Emit the release functions if they're used. This is wrapped in .ifndef to
; prevent duplicate labels when LTO concatenates module asm from multiple
; translation units.
.if NEED_RELEASE

.ifndef _swift_releaseDirect

.weak_definition _swift_bridgeObjectReleaseDirect
.private_extern _swift_bridgeObjectReleaseDirect
.weak_definition _swift_releaseDirect
.private_extern _swift_releaseDirect

.if OBJC_INTEROP

_swift_bridgeObjectReleaseDirect:
  ; If it's a tagged pointer, then we don't do anything.
  tbz  x0, #63, LbridgeObjectReleaseNotTagged
  ret
LbridgeObjectReleaseNotTagged:
  ; If the ObjC bit is set, call objc_release. Otherwise, mask out the
  ; non-pointer bits and fall through to swift_releaseDirect.
  tbnz  x0, #62, LbridgeObjectReleaseDirectObjC
  and   x0, x0, BRIDGEOBJECT_POINTER_BITS

.else ; !OBJC_INTEROP

_swift_bridgeObjectReleaseDirect:
  ; Mask out the non-pointer bits and fall through to swift_releaseDirect.
  and   x0, x0, BRIDGEOBJECT_POINTER_BITS

.endif ; OBJC_INTEROP

; Ensure that the fallthrough into swift_releaseDirect works.
.alt_entry _swift_releaseDirect
_swift_releaseDirect:

  ; RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lrelease_ret

  ; We'll operate on the address of the refcount field, which is 8 bytes into
  ; the object.
  add   x1, x0, #8

  ; Load the current value in the refcount field when using CAS.
.if USE_CAS
  ldr x16, [x1]
.endif

  ; The compare-and-swap goes back to here when it needs to retry.
Lrelease_retry:

  ; Get the slow path mask and see if the refcount field has any of those bits
  ; set.
  adrp  x17, ___retainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, ___retainRelease_slowpath_mask@PAGEOFF]

  ; Load-exclusive of the current value in the refcount field when using LLSC.
  ; stxr does not update x16 like cas does, so this load must be inside the
  ; loop. ldxr/stxr are not guaranteed to make forward progress if there are
  ; memory accesses between them, so we need to do this after getting the mask
  ; above.
.if USE_CAS == 0
  ldxr x16, [x1]
.endif

  tst   x16, x17

  ; Also check if we're releasing with a refcount of 0. That will initiate
  ; dealloc and requires calling the slow path. We don't try to decrement and
  ; then call dealloc in that case. We'll just immediately go to the slow path
  ; and let it take care of the entire operation.
  mov   x17, #STRONG_RC_ONE
  ccmp  x16, x17, #0x8, eq

  ; If the refcount value matches the slow path mask, or the strong refcount
  ; is zero, then go to the slow path.
  b.lt  Lslowpath_release

  ; We're good to proceed with the fast path. Compute the new value of the
  ; refcount field.
  sub   x17, x16, x17

.if USE_CAS
  ; Save a copy of the old value so we can determine if the CAS succeeded.
  mov   x2, x16

  ; Compare and swap the new value into the refcount field. Perform the
  ; operation with release memory ordering so that dealloc on another thread
  ; will see all stores performed on this thread prior to calling release.
  casl  x16, x17, [x1]

  ; The previous value of the refcount field is now in x16. We succeeded if
  ; that value is the same as the old value we had before. If we failed, retry.
  cmp   x2, x16
  b.ne  Lrelease_retry
.else ; !USE_CAS
  ; Try to store the updated value.
  stlxr  w16, x17, [x1]

  ; On failure, retry.
  cbnz  w16, Lrelease_retry
.endif ; USE_CAS

  ; On success, return.
Lrelease_ret:
  ret

Lslowpath_release:
.if USE_CAS == 0
  clrex
.endif
.if TARGET_HAS_PRESERVEMOST
  ; The deployment target guarantees this symbol exists.
  b _swift_release_preservemost
.else
  ; If the weak preservemost symbol is NULL, call our helper. Otherwise call
  ; the runtime directly.
  adrp x17, _swift_release_preservemost@GOTPAGE
  ldr  x17, [x17, _swift_release_preservemost@GOTPAGEOFF]
  cbz x17, Lcall_swift_release
  b _swift_release_preservemost
  ; Branch to the IR helper function which saves/restores preserve_most
  ; registers and calls the runtime.
Lcall_swift_release:
  b _swift_release_callframe
.endif ; TARGET_HAS_PRESERVEMOST

  ; ObjC release path.
.if OBJC_INTEROP
LbridgeObjectReleaseDirectObjC:
  b _swift_objc_release_callframe
.endif ; OBJC_INTEROP

.endif ; .ifndef _swift_releaseDirect

.endif ; NEED_RELEASE


; =============================================================================
; Retain
; =============================================================================

; Emit retain functions if they're used. Like with release, we wrap it in a
; .ifndef to avoid multiple definitions.
.if NEED_RETAIN

.ifndef _swift_retainDirect

.weak_definition _swift_bridgeObjectRetainDirect
.private_extern _swift_bridgeObjectRetainDirect
.weak_definition _swift_retainDirect
.private_extern _swift_retainDirect

.if OBJC_INTEROP

_swift_bridgeObjectRetainDirect:
  ; If it's a tagged pointer, then we don't do anything.
  tbz  x0, #63, LbridgeObjectRetainNotTagged
  ret
LbridgeObjectRetainNotTagged:
  ; If the ObjC bit is set, call objc_retain. Otherwise, fall through to
  ; swift_retainDirect, which will tolerate and preserve any nonpointer bits
  ; set in x0.
  tbnz  x0, #62, Lswift_bridgeObjectRetainDirectObjC

.alt_entry _swift_retainDirect

.else ; !OBJC_INTEROP

  ; When there's no ObjC interop, then swift_retainDirect works for
  ; bridgeObjects too.
  .set _swift_bridgeObjectRetainDirect, _swift_retainDirect

.endif ; OBJC_INTEROP

_swift_retainDirect:

  ; RR of NULL or values with high bit set is a no-op.
  cmp   x0, #0
  b.le  Lretain_ret

  ; Mask off spare bits that may have come in from bridgeObjectRetain. Keep the
  ; original value in x0 since we have to return it.
  and   x1, x0, BRIDGEOBJECT_POINTER_BITS

  ; We'll operate on the address of the refcount field, which is 8 bytes into
  ; the object.
  add   x1, x1, #8

  ; Load the current value of the refcount field when using CAS.
.if USE_CAS
  ldr   x16, [x1]
.endif

Lretain_retry:

  ; Get the slow path mask and see if the refcount field has any of those bits
  ; set.
  adrp  x17, ___retainRelease_slowpath_mask@PAGE
  ldr   x17, [x17, ___retainRelease_slowpath_mask@PAGEOFF]

  ; Load-exclusive of the current value in the refcount field when using LLSC.
  ; stxr does not update x16 like cas does, so this load must be inside the
  ; loop. ldxr/stxr are not guaranteed to make forward progress if there are
  ; memory accesses between them, so we need to do this after getting the mask
  ; above.
.if USE_CAS == 0
  ldxr x16, [x1]
.endif

  ; Compute a refcount field with the strong refcount incremented.
  mov   x3, #STRONG_RC_ONE
  add   x3, x16, x3

  ; Test the incremented value against the slowpath mask. This checks for both
  ; the side table case and the overflow case, as the overflow sets the high
  ; bit.
  tst   x3, x17
  b.ne  Lslowpath_retain

.if USE_CAS
  ; Save the old value so we can check if the CAS succeeded.
  mov   x2, x16

  ; Compare and swap the new value into the refcount field. Retain can use
  ; relaxed memory ordering.
  cas   x16, x3, [x1]

  ; The previous value of the refcount field is now in x16. We succeeded if
  ; that value is the same as the old value we had before. If we failed, retry.
  cmp   x2, x16
  b.ne  Lretain_retry
.else ; !USE_CAS
  ; Try to store the updated value.
  stxr  w16, x3, [x1]

  ; On failure, retry.
  cbnz  w16, Lretain_retry
.endif ; USE_CAS

  ; If the store succeeds, return. Retain returns the object pointer being
  ; retained, which is still in x0 at this point. bridgeObjectRetain returns
  ; the unmasked pointer, which we've preserved in x0.
Lretain_ret:
  ret

Lslowpath_retain:
.if USE_CAS == 0
  clrex
.endif
.if TARGET_HAS_PRESERVEMOST
  ; The deployment target guarantees this symbol exists.
  b _swift_retain_preservemost
.else
  ; If the weak preservemost symbol is NULL, call our helper. Otherwise call
  ; the runtime directly.
  adrp x17, _swift_retain_preservemost@GOTPAGE
  ldr  x17, [x17, _swift_retain_preservemost@GOTPAGEOFF]
  cbz x17, Lcall_swift_retain
  b _swift_retain_preservemost
  ; Branch to the IR helper function which saves/restores preserve_most
  ; registers and calls the runtime.
Lcall_swift_retain:
  b _swift_retain_callframe
.endif ; TARGET_HAS_PRESERVEMOST

  ; ObjC retain path.
.if OBJC_INTEROP
Lswift_bridgeObjectRetainDirectObjC:
  b _swift_objc_retain_callframe
.endif ; OBJC_INTEROP

.endif ; .ifndef _swift_retainDirect

.endif ; NEED_RETAIN
