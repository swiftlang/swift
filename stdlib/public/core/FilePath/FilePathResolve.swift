/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

#if FILEPATH_PACKAGE
#if os(Windows)
import WinSDK
#elseif canImport(Darwin)
import Darwin
#elseif os(Linux) || os(Android) || os(FreeBSD) || os(OpenBSD) || os(WASI)
import Glibc
#endif
#else
// Stdlib build: SwiftShims supplies `_swift_stdlib_FilePath_resolve` (the
// platform-specific resolve syscall, in stubs/FilePathStubs.cpp),
// `_swift_stdlib_free`, and `__swift_size_t`.
import SwiftShims
#endif

// MARK: - Internal error type
//
// Carries a platform error code (`errno` on POSIX, `GetLastError()` on
// Windows) so callers can introspect via `as? _FilePathResolveError` while
// `resolve()`'s public surface stays untyped (`throws`).

@available(SwiftStdlib 9999, *)
internal struct _FilePathResolveError: Error {
  internal var code: CInt
}

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Resolve this path against the filesystem, producing an absolute
  /// path with all symbolic links and `.`/`..` components resolved.
  ///
  /// All intermediate components must exist. Throws if the path
  /// cannot be resolved.
  ///
  /// This operation is synchronous and may block.
  @available(*, noasync)
  @available(SwiftStdlib 9999, *)
  public func resolve() throws -> FilePath {
#if FILEPATH_PACKAGE
#if os(Windows)
    return try _resolveWindows()
#elseif canImport(Darwin)
    return try _resolveDarwin()
#else
    return try _resolveLinux()
#endif
#else
    // Stdlib port: dispatch to the runtime stub, which is the only place in
    // the stdlib build that imports platform headers. See
    // `swift/stdlib/public/SwiftShims/swift/shims/FilePath.h` and
    // `swift/stdlib/public/stubs/FilePathStubs.cpp`.
    return try _resolveViaStdlibStub()
#endif
  }
}

#if !FILEPATH_PACKAGE

@available(SwiftStdlib 9999, *)
extension FilePath {
  fileprivate func _resolveViaStdlibStub() throws -> FilePath {
    var outBuf: UnsafeMutablePointer<FilePath.CodeUnit>? = nil
    var outCount: __swift_size_t = 0
    let err: CInt = unsafe self.withCodeUnits { ptr, count in
      unsafe _swift_stdlib_FilePath_resolve(
        ptr, __swift_size_t(count), &outBuf, &outCount)
    }
    guard err == 0, let resultBuf = unsafe outBuf else {
      throw _FilePathResolveError(code: err)
    }
    defer { unsafe _swift_stdlib_free(resultBuf) }
    return unsafe FilePath(
      _normalizingRawCodeUnits: UnsafeRawPointer(resultBuf),
      count: Int(outCount))
  }
}

#endif // !FILEPATH_PACKAGE

// Build a FilePath from `count` platform code units starting at `ptr`.
// The bytes must already be a valid path (no embedded NUL); the result
// is fed through `_normalizing`, which canonicalizes whatever
// per-platform anchor / suffix the bytes happen to express. One
// allocation (the `_SystemString` storage) plus one bulk copy — the
// backing `Array` is sized exactly and filled in place rather than via
// per-element `append`.
@available(SwiftStdlib 9999, *)
extension FilePath {
  fileprivate init(
    _normalizingRawCodeUnits ptr: UnsafeRawPointer,
    count: Int
  ) {
    _internalInvariant(count >= 0)
    let chars = unsafe Array<FilePath.CodeUnit>(
      unsafeUninitializedCapacity: count + 1
    ) { buf, initializedCount in
      if count > 0 {
        let byteCount = count * MemoryLayout<FilePath.CodeUnit>.stride
        unsafe UnsafeMutableRawPointer(buf.baseAddress!)
          .copyMemory(from: ptr, byteCount: byteCount)
      }
      unsafe buf[count] = ._null
      initializedCount = count + 1
    }
    self.init(_normalizing: _SystemString(nullTerminated: chars))
  }
}

#if FILEPATH_PACKAGE

// MARK: - Per-platform implementation

#if os(Windows)

// MARK: Windows

@available(SwiftStdlib 9999, *)
extension FilePath {
  fileprivate func _resolveWindows() throws -> FilePath {
    let shareMode: DWORD =
      DWORD(FILE_SHARE_READ) | DWORD(FILE_SHARE_WRITE) | DWORD(FILE_SHARE_DELETE)

    let handle: HANDLE = self._storage.withNullTerminatedCodeUnits { p in
      unsafe CreateFileW(
        p.baseAddress,
        0,
        shareMode,
        nil,
        DWORD(OPEN_EXISTING),
        DWORD(FILE_FLAG_BACKUP_SEMANTICS),
        nil)
    }
    if handle == INVALID_HANDLE_VALUE {
      throw _FilePathResolveError(code: CInt(GetLastError()))
    }
    defer { unsafe _ = CloseHandle(handle) }

    let flags: DWORD = DWORD(FILE_NAME_NORMALIZED) | DWORD(VOLUME_NAME_DOS)
    var capacity: Int = 1024
    while true {
      var buf = [WCHAR](repeating: 0, count: capacity)
      let needed: DWORD = unsafe buf.withUnsafeMutableBufferPointer { ptr in
        unsafe GetFinalPathNameByHandleW(
          handle, ptr.baseAddress, DWORD(ptr.count), flags)
      }
      if needed == 0 {
        throw _FilePathResolveError(code: CInt(GetLastError()))
      }
      // On success `needed` is the length WITHOUT the NUL (so it fits with
      // room to spare). On buffer-too-small it's the required size INCLUDING
      // the NUL — grow and retry.
      if Int(needed) < capacity {
        return unsafe buf.withUnsafeBufferPointer { ptr in
          unsafe FilePath(
            _normalizingRawCodeUnits: UnsafeRawPointer(ptr.baseAddress!),
            count: Int(needed))
        }
      }
      capacity = Int(needed) + 1
    }
  }
}

#elseif canImport(Darwin)

// MARK: Darwin
//
// realpath(3) returns the un-firmlinked underlay (e.g.
// `/System/Volumes/Data/Users/foo`) for paths whose components live on the
// data volume but are exposed via firmlinks (the typical case for `/Users`
// on macOS Big Sur+). The kernel's *default* behavior for
// `ATTR_CMN_FULLPATH` returns the firmlinked path instead — pinned for
// this build by `ResolveTests.firmlinkedHomeRealPath`. So we use
// `getattrlistat` directly with `ATTR_CMN_FULLPATH` and avoid `realpath`
// entirely.

@available(SwiftStdlib 9999, *)
extension FilePath {
  fileprivate func _resolveDarwin() throws -> FilePath {
    // Two attempts: 8 KiB suits any normal path, 32 KiB covers
    // long-path-enabled processes. Past that, the path is genuinely
    // too long.
    for size in [8192, 32768] {
      if let resolved = try _resolveDarwinAttempt(bufferSize: size) {
        return resolved
      }
    }
    throw _FilePathResolveError(code: ENAMETOOLONG)
  }

  // Returns nil ONLY when the buffer was too small and the caller should
  // retry with a larger one (ERANGE / ENAMETOOLONG). Other errors throw.
  private func _resolveDarwinAttempt(
    bufferSize: Int
  ) throws -> FilePath? {
    var attrs = attrlist()
    attrs.bitmapcount = UInt16(ATTR_BIT_MAP_COUNT)
    attrs.commonattr = attrgroup_t(ATTR_CMN_FULLPATH)

    let options: CUnsignedLong =
      CUnsignedLong(FSOPT_ATTR_CMN_EXTENDED) |
      CUnsignedLong(FSOPT_RETURN_REALDEV)

    let bufRaw = UnsafeMutableRawPointer.allocate(
      byteCount: bufferSize,
      alignment: MemoryLayout<UInt32>.alignment)
    defer { unsafe bufRaw.deallocate() }

    let rc: CInt = unsafe self._storage.withNullTerminatedCodeUnits { pathBuf in
      unsafe withUnsafeMutablePointer(to: &attrs) { attrsPtr in
        unsafe getattrlistat(
          AT_FDCWD,
          pathBuf.baseAddress,
          attrsPtr,
          bufRaw,
          bufferSize,
          options)
      }
    }
    if rc != 0 {
      let err = errno
      if err == ERANGE || err == ENAMETOOLONG {
        return nil
      }
      throw _FilePathResolveError(code: err)
    }

    // Buffer layout when ATTR_CMN_FULLPATH is the only attribute requested:
    //   buf[0..4]   u_int32_t total bytes used (we ignore this header)
    //   buf[4..12]  attrreference_t for the FULLPATH attribute
    //                 .attr_dataoffset  offset relative to the start of
    //                                   the attrreference_t struct itself
    //                 .attr_length      bytes (includes trailing NUL)
    //   buf[4 + .attr_dataoffset ..]    the resolved C-string path
    let attrrefOffset = MemoryLayout<UInt32>.size
    let attrref: attrreference_t = unsafe bufRaw
      .advanced(by: attrrefOffset)
      .load(as: attrreference_t.self)
    let stringStart = unsafe bufRaw
      .advanced(by: attrrefOffset + Int(attrref.attr_dataoffset))
    let storedLength = Int(attrref.attr_length)

    // ATTR_CMN_FULLPATH is documented as a null-terminated string and
    // attr_length includes the NUL. Both are kernel guarantees; assert them
    // rather than handling a "no trailing NUL" branch that can't fire.
    _internalInvariant(storedLength > 0)
    _internalInvariant(
      unsafe stringStart.load(
        fromByteOffset: storedLength - 1, as: UInt8.self) == 0,
      "ATTR_CMN_FULLPATH must be null-terminated")

    return unsafe FilePath(
      _normalizingRawCodeUnits: stringStart, count: storedLength - 1)
  }
}

#else

// MARK: Linux (and other POSIX)
//
// Linux has neither firmlinks nor Darwin-style anchor prefixes, so the
// portable POSIX call is correct.

@available(SwiftStdlib 9999, *)
extension FilePath {
  fileprivate func _resolveLinux() throws -> FilePath {
    let resolved = self._storage.withNullTerminatedCodeUnits { p in
      unsafe realpath(p.baseAddress, nil)
    }
    guard let resolved = resolved else {
      throw _FilePathResolveError(code: errno)
    }
    defer { unsafe free(resolved) }

    let length = unsafe strlen(resolved)
    return unsafe FilePath(_normalizingRawCodeUnits: resolved, count: length)
  }
}

#endif

#endif // FILEPATH_PACKAGE
