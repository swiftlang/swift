// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/pass_object_size.h) | %FileCheck %s
// REQUIRES: executable_test

// End-to-end check that the callee actually receives the implicit size
// argument. For a pointer whose provenance the optimizer cannot see, Clang's
// answer -- and therefore ours -- is "unknown": SIZE_MAX for a max query and 0
// for a min query. `size_t` imports as `Int`, so SIZE_MAX prints as -1.
//
// Before this was implemented the callee received the *pointer bits* in that
// slot, so these values are what distinguish a correct call from the old
// miscompile.

func opaque(_ p: UnsafeMutablePointer<CChar>) -> UnsafeMutablePointer<CChar> {
  return p
}

var storage = [CChar](repeating: 0, count: 24)

storage.withUnsafeMutableBufferPointer { buf in
  let p = opaque(buf.baseAddress!)

  // CHECK: max: -1
  print("max: \(pos_report_max(p))")

  // CHECK-NEXT: min: 0
  print("min: \(pos_report_min(p))")

  // CHECK-NEXT: second: -1
  print("second: \(pos_report_second(p, p, 7))")

  // SIZE_MAX ^ 0 == SIZE_MAX
  // CHECK-NEXT: both: -1
  print("both: \(pos_report_both(p, p))")
}

// Referencing the function as a value goes through a foreign-to-native thunk.
// The thunk sees an opaque pointer, so the answer is still "unknown" -- but it
// must be a *size*, not the pointer.
let asValue: (UnsafeMutablePointer<CChar>?) -> Int = pos_report_max
storage.withUnsafeMutableBufferPointer { buf in
  // CHECK-NEXT: thunked: -1
  print("thunked: \(asValue(opaque(buf.baseAddress!)))")
}

// Forming a plain C function pointer requires a thunk, because the entry point
// expects an implicit size argument that a function pointer cannot carry. The
// thunk sees an opaque pointer, so it passes "unknown" -- but it is a real C
// function pointer, callable from C.
let viaPointer: @convention(c) (UnsafeMutablePointer<CChar>?) -> Int = pos_report_max
storage.withUnsafeMutableBufferPointer { buf in
  // CHECK-NEXT: through C pointer: -1
  print("through C pointer: \(pos_call_through(viaPointer, buf.baseAddress!))")
}
