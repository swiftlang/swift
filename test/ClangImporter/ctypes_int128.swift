// RUN: %target-swift-frontend -enable-objc-interop -import-objc-header %S/Inputs/ctypes_int128.h -typecheck -verify %s
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -import-objc-header %S/Inputs/ctypes_int128.h -typecheck -verify %s

// REQUIRES: PTRSIZE=64

@available(SwiftStdlib 6.0, *)
func test_int128() {
  let x: CInt128 = returns_int128()
  let _: Int128 = x
  takes_int128(x)
}

@available(SwiftStdlib 6.0, *)
func test_uint128() {
  let x: CUnsignedInt128 = returns_uint128()
  let _: UInt128 = x
  takes_uint128(x)
}
