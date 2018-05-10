// RUN: %target-swift-frontend -typecheck -verify %s

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
  typealias JumpBuffer = Int32
#elseif os(Android) || os(Cygwin) || os(FreeBSD) || os(Linux)
  import Glibc
  typealias JumpBuffer = jmp_buf
#elseif os(Windows)
  import MSVCRT
  typealias JumpBuffer = jmp_buf
#endif

func test_unavailable_returns_twice_function() {
  var x: JumpBuffer
  _ = setjmp(&x) // expected-error {{'setjmp' is unavailable: Functions that may return more than one time (annotated with the 'returns_twice' attribute) are unavailable in Swift}}
}

