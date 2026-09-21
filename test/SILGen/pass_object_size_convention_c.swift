// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/pass_object_size.h -verify %s | %FileCheck %s

// A @convention(c) function pointer has nowhere to carry the implicit size
// argument, so it cannot point at the imported entry point directly. Clang
// rejects taking the address of such a function outright; Swift forwards
// through a thunk instead. Inside the thunk the pointer is opaque, so the size
// is the conservative "unknown" -- the same value C computes when it cannot see
// the allocation, and the same value the foreign-to-native thunk already passes
// when the function is referenced as a Swift closure.

func takesCPointer(_ f: @convention(c) (UnsafeMutablePointer<CInt>?) -> Void) {}
func takesClosure(_ f: (UnsafeMutablePointer<CInt>?) -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s29pass_object_size_convention_c19testAsCPointerThunkyyF
func testAsCPointerThunk() {
  // The thunk, not the C entry point, is what gets passed along.
  // CHECK: function_ref @$sSo7pos_maxyySpys5Int32VGSgFTwi
  takesCPointer(pos_max)
}

// The thunk drops the parameter flags from its own signature -- that is what
// makes it a function pointer C can hold -- and forwards to the entry point that
// still has them, so IRGen supplies the size there.
// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo7pos_maxyySpys5Int32VGSgFTwi : $@convention(c) (Optional<UnsafeMutablePointer<Int32>>) -> ()
// CHECK: [[C:%.*]] = function_ref @$sSo7pos_maxyySpys5Int32VGSgFTo : $@convention(c) (@sil_pass_object_size Optional<UnsafeMutablePointer<Int32>>) -> ()
// CHECK: apply [[C]]

// An unannotated function needs no thunk at all.
// CHECK-LABEL: sil hidden [ossa] @$s29pass_object_size_convention_c21testCPointerFromPlainyyF
func testCPointerFromPlain() {
  // CHECK-NOT: Twi
  // CHECK: function_ref @$sSo9pos_plainyySpys5Int32VGSgFTo
  takesCPointer(pos_plain)
}

// Native Swift function values keep going through the foreign-to-native thunk,
// which already passed a conservative size before any of this.
// CHECK-LABEL: sil hidden [ossa] @$s29pass_object_size_convention_c17testAsNativeValueyyF
func testAsNativeValue() {
  // CHECK: function_ref @$sSo7pos_maxyySpys5Int32VGSgFTO
  takesClosure(pos_max)
}

// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo7pos_maxyySpys5Int32VGSgFTO : $@convention(thin) (Optional<UnsafeMutablePointer<Int32>>) -> ()
// CHECK: [[C2:%.*]] = function_ref @$sSo7pos_maxyySpys5Int32VGSgFTo : $@convention(c) (@sil_pass_object_size Optional<UnsafeMutablePointer<Int32>>) -> ()
// CHECK: apply [[C2]]
