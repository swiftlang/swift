; RUN: %swift-llvm-opt -passes='swift-merge-functions' -swiftmergefunc-threshold=4 %s | %FileCheck %s
;
; A call site can legitimately be emitted with a different (shorter) function
; type than the callee definition. This happens with `@_silgen_name`
; declarations whose C prototype omits part of the Swift ABI -- e.g. the
; indirect error result of a function that has a large typed `throws`. Such a
; call is well-formed: the extra result buffer is only accessed on the throw
; path, so the caller need not materialize it.
;
; SwiftMergeFunctions appends the differing constant as a new parameter at the
; position defined by the callee. If it rewrote such a mismatched call site, the
; constant would land in the wrong argument slot -- the merged callee reads it
; from a different register than the caller passes it in, which crashes at
; runtime. Verify the pass keeps a thunk with the definition's signature instead
; of rewriting the mismatched call.

declare void @worker_a(ptr, i64)
declare void @worker_b(ptr, i64)

; The callers invoke the definitions through a two-parameter type, omitting the
; trailing %err parameter. They are structurally distinct so they are not
; merged with each other, which keeps the mismatched *direct* call in place.

; CHECK-LABEL: define i64 @caller_a(
; CHECK: call i64 @impl_a(ptr %x, i64 %y)
; CHECK-NOT: Tm(
define i64 @caller_a(ptr %x, i64 %y) {
  %r = call i64 @impl_a(ptr %x, i64 %y)
  ret i64 %r
}

; CHECK-LABEL: define i64 @caller_b(
; CHECK: call i64 @impl_b(ptr %x, i64 %y)
; CHECK-NOT: Tm(
define i64 @caller_b(ptr %x, i64 %y) {
  %r = call i64 @impl_b(ptr %x, i64 %y)
  %s = mul i64 %r, %r
  ret i64 %s
}

; The definitions take a trailing %err parameter that models an indirect error
; result. They differ only by which worker they call, so they are merged. Each
; original is kept as a thunk that forwards all three of its own parameters plus
; the worker constant, in the correct position.

; CHECK-LABEL: define internal i64 @impl_a(ptr %p, i64 %imm, ptr %err)
; CHECK: tail call i64 @impl_aTm(ptr %p, i64 %imm, ptr %err, ptr @worker_a)
define internal i64 @impl_a(ptr %p, i64 %imm, ptr %err) {
  %v = load i64, ptr %p, align 8
  call void @worker_a(ptr %err, i64 %v)
  %n = add i64 %v, %imm
  ret i64 %n
}

; CHECK-LABEL: define internal i64 @impl_b(ptr %p, i64 %imm, ptr %err)
; CHECK: tail call i64 @impl_aTm(ptr %p, i64 %imm, ptr %err, ptr @worker_b)
define internal i64 @impl_b(ptr %p, i64 %imm, ptr %err) {
  %v = load i64, ptr %p, align 8
  call void @worker_b(ptr %err, i64 %v)
  %n = add i64 %v, %imm
  ret i64 %n
}

; CHECK-LABEL: define internal i64 @impl_aTm(ptr %0, i64 %1, ptr %2, ptr %3)
; CHECK: call void %3(ptr %2, i64 %v)
