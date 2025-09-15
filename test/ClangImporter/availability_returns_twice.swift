// RUN: %target-typecheck-verify-swift

// UNSUPPORTED: OS=windows-msvc

// `setjmp` is not available on WebAssembly/WASI
// UNSUPPORTED: OS=wasip1

// https://github.com/apple/swift/issues/51632
// In Android jmp_buf is int[16], which doesn't convert to &Int
// XFAIL: OS=linux-androideabi
// XFAIL: OS=linux-android
// XFAIL: OS=openbsd

#if canImport(setjmp_h)
  import setjmp_h
  typealias JumpBuffer = Int32
#elseif canImport(Darwin)
  import Darwin
  typealias JumpBuffer = Int32
#elseif canImport(Glibc)
  import Glibc
  typealias JumpBuffer = jmp_buf
#else
#error("Unsupported platform")
#endif

func test_unavailable_returns_twice_function() {
  var x: JumpBuffer
  _ = setjmp(&x) // expected-error {{'setjmp' is unavailable: Functions that may return more than one time (annotated with the 'returns_twice' attribute) are unavailable in Swift}}
}

