//===--- EmbeddedReadLine.swift - readLine() backing for Embedded Swift ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_HAS_STDIN

// This reproduces the full stdlib's POSIX `swift_stdlib_readLine_stdin`
// (the non-`_WIN32` branch in stdlib/public/stubs/Stubs.cpp): `getline` over
// `stdin`, retrying while it fails with `errno == EINTR`. Embedded Swift has no
// stdio.h/errno.h import, so the libc symbols the C version reaches through the
// preprocessor must be named explicitly here. Two are spelled differently per
// libc, and the `EINTR` value differs too:
//   - the `stdin` FILE* global:  `__stdinp` on Darwin, `stdin` elsewhere
//   - the errno accessor:        `__error()` on Darwin, `__errno_location()` elsewhere
//   - EINTR:                     27 on wasi-libc, 4 on Darwin/Linux/musl

@_extern(c, "getline")
func getline(
  _ linePtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
  _ capacity: UnsafeMutablePointer<Int>,
  _ stream: OpaquePointer?
) -> Int

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
@_extern(c, "__stdinp")
var _stdin: OpaquePointer?
@_extern(c, "__error")
func _errnoLocation() -> UnsafeMutablePointer<CInt>
#else // linux-gnu (glibc/musl), wasip1, emscripten
@_extern(c, "stdin")
var _stdin: OpaquePointer?
@_extern(c, "__errno_location")
func _errnoLocation() -> UnsafeMutablePointer<CInt>
#endif

#if os(WASI)
let _EINTR: CInt = 27
#else
let _EINTR: CInt = 4
#endif

/// Backing for `readLine()` in Embedded Swift on hosted-libc targets.
///
/// A direct reproduction of the full stdlib's POSIX implementation
/// (`swift_stdlib_readLine_stdin`, stdlib/public/stubs/Stubs.cpp): read one line
/// from `stdin` with `getline` into a `malloc`'d buffer (which the caller
/// releases with `free`, matching `_swift_stdlib_free`), retrying while `getline`
/// fails with `errno == EINTR`. Returns the number of bytes read including any
/// trailing newline, or -1 at EOF. `linePtr` must be non-null (the C declaration
/// annotates it `_Nonnull`); on success it receives the `getline`-allocated
/// buffer.
@c(swift_stdlib_readLine_stdin)
public func swift_stdlib_readLine_stdin(
  _ linePtr: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>
) -> Int {
  var capacity = 0
  while true {
    let result = unsafe getline(linePtr, &capacity, _stdin)
    // Retry only on an EINTR-interrupted read, exactly as the C version's
    // `do { } while (result < 0 && errno == EINTR)`: a success or any other
    // failure (including EOF, where getline returns -1) is returned as-is.
    if result >= 0 { return result }
    if unsafe _errnoLocation().pointee != _EINTR { return result }
  }
}

#endif // SWIFT_STDLIB_HAS_STDIN
