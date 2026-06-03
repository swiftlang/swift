; RUN: %swift-llvm-opt -passes='swift-merge-functions' -swiftmergefunc-threshold=2 %s | %FileCheck %s
;
; Verify that SwiftMergeFunctions does not merge functions containing
; musttail calls.

target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios18.0"

; CHECK-LABEL: define linkonce_odr hidden ptr @thunk_a(
; CHECK: musttail call ptr @impl_a(
; CHECK: ret ptr
define linkonce_odr hidden ptr @thunk_a(ptr noundef %self) unnamed_addr {
entry:
  %sel = load ptr, ptr @selector_a, align 8, !invariant.load !0
  %realized = tail call ptr @objc_msgSend(ptr noundef %self, ptr noundef %sel)
  %ret = musttail call ptr @impl_a(ptr noundef %self)
  ret ptr %ret
}

; CHECK-LABEL: define linkonce_odr hidden ptr @thunk_b(
; CHECK: musttail call ptr @impl_b(
; CHECK: ret ptr
define linkonce_odr hidden ptr @thunk_b(ptr noundef %self) unnamed_addr {
entry:
  %sel = load ptr, ptr @selector_b, align 8, !invariant.load !0
  %realized = tail call ptr @objc_msgSend(ptr noundef %self, ptr noundef %sel)
  %ret = musttail call ptr @impl_b(ptr noundef %self)
  ret ptr %ret
}

; CHECK-LABEL: define linkonce_odr hidden ptr @thunk_c(
; CHECK: musttail call ptr @impl_c(
; CHECK: ret ptr
define linkonce_odr hidden ptr @thunk_c(ptr noundef %self) unnamed_addr {
entry:
  %sel = load ptr, ptr @selector_c, align 8, !invariant.load !0
  %realized = tail call ptr @objc_msgSend(ptr noundef %self, ptr noundef %sel)
  %ret = musttail call ptr @impl_c(ptr noundef %self)
  ret ptr %ret
}

@selector_a = external global ptr
@selector_b = external global ptr
@selector_c = external global ptr

declare ptr @objc_msgSend(ptr, ptr, ...)
declare ptr @impl_a(ptr)
declare ptr @impl_b(ptr)
declare ptr @impl_c(ptr)

!0 = !{}
