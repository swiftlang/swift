//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims
import SwiftOverlayShims

//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

public var errno : Int32 {
  get {
    return _swift_stdlib_getErrno()
  }
  set(val) {
    return _swift_stdlib_setErrno(val)
  }
}


//===----------------------------------------------------------------------===//
// stdio.h
//===----------------------------------------------------------------------===//

#if os(FreeBSD) || os(PS4)
public var stdin : UnsafeMutablePointer<FILE> {
  get {
    return __stdinp
  }
  set {
    __stdinp = newValue
  }
}

public var stdout : UnsafeMutablePointer<FILE> {
  get {
    return __stdoutp
  }
  set {
    __stdoutp = newValue
  }
}

public var stderr : UnsafeMutablePointer<FILE> {
  get {
    return __stderrp
  }
  set {
    __stderrp = newValue
  }
}

#if !$Embedded
public func dprintf(_ fd: Int, _ format: UnsafePointer<Int8>, _ args: CVarArg...) -> Int32 {
  return withVaList(args) { va_args in
    vdprintf(Int32(fd), format, va_args)
  }
}

public func snprintf(ptr: UnsafeMutablePointer<Int8>, _ len: Int, _ format: UnsafePointer<Int8>, _ args: CVarArg...) -> Int32 {
  return withVaList(args) { va_args in
    return vsnprintf(ptr, len, format, va_args)
  }
}
#endif

#elseif os(OpenBSD)
public var stdin: UnsafeMutablePointer<FILE> { return _swift_stdlib_stdin() }
public var stdout: UnsafeMutablePointer<FILE> { return _swift_stdlib_stdout() }
public var stderr: UnsafeMutablePointer<FILE> { return _swift_stdlib_stderr() }
#elseif os(Windows)
public var stdin: UnsafeMutablePointer<FILE> { return __acrt_iob_func(0) }
public var stdout: UnsafeMutablePointer<FILE> { return __acrt_iob_func(1) }
public var stderr: UnsafeMutablePointer<FILE> { return __acrt_iob_func(2) }

public var STDIN_FILENO: Int32 { return _fileno(stdin) }
public var STDOUT_FILENO: Int32 { return _fileno(stdout) }
public var STDERR_FILENO: Int32 { return _fileno(stderr) }
#endif


//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32
) -> Int32 {
  return _swift_stdlib_open(path, oflag, 0)
}

#if os(Windows)
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: Int32
) -> Int32 {
  return _swift_stdlib_open(path, oflag, mode)
}
#else
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32 {
  return _swift_stdlib_open(path, oflag, mode)
}

public func openat(
  _ fd: Int32,
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32
) -> Int32 {
  return _swift_stdlib_openat(fd, path, oflag, 0)
}

public func openat(
  _ fd: Int32,
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32 {
  return _swift_stdlib_openat(fd, path, oflag, mode)
}

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32
) -> Int32 {
  return _swift_stdlib_fcntl(fd, cmd, 0)
}

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32,
  _ value: Int32
) -> Int32 {
  return _swift_stdlib_fcntl(fd, cmd, value)
}

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32,
  _ ptr: UnsafeMutableRawPointer
) -> Int32 {
  return _swift_stdlib_fcntlPtr(fd, cmd, ptr)
}

// !os(Windows)
#endif

#if os(Windows)
public var S_IFMT: Int32 { return Int32(0xf000) }

public var S_IFREG: Int32 { return Int32(0x8000) }
public var S_IFDIR: Int32 { return Int32(0x4000) }
public var S_IFCHR: Int32 { return Int32(0x2000) }
public var S_IFIFO: Int32 { return Int32(0x1000) }

public var S_IREAD: Int32  { return Int32(0x0100) }
public var S_IWRITE: Int32 { return Int32(0x0080) }
public var S_IEXEC: Int32  { return Int32(0x0040) }
#else
public var S_IFMT: mode_t   { return mode_t(0o170000) }
public var S_IFIFO: mode_t  { return mode_t(0o010000) }
public var S_IFCHR: mode_t  { return mode_t(0o020000) }
public var S_IFDIR: mode_t  { return mode_t(0o040000) }
public var S_IFBLK: mode_t  { return mode_t(0o060000) }
public var S_IFREG: mode_t  { return mode_t(0o100000) }
public var S_IFLNK: mode_t  { return mode_t(0o120000) }
public var S_IFSOCK: mode_t { return mode_t(0o140000) }

public var S_IRWXU: mode_t  { return mode_t(0o000700) }
public var S_IRUSR: mode_t  { return mode_t(0o000400) }
public var S_IWUSR: mode_t  { return mode_t(0o000200) }
public var S_IXUSR: mode_t  { return mode_t(0o000100) }

public var S_IRWXG: mode_t  { return mode_t(0o000070) }
public var S_IRGRP: mode_t  { return mode_t(0o000040) }
public var S_IWGRP: mode_t  { return mode_t(0o000020) }
public var S_IXGRP: mode_t  { return mode_t(0o000010) }

public var S_IRWXO: mode_t  { return mode_t(0o000007) }
public var S_IROTH: mode_t  { return mode_t(0o000004) }
public var S_IWOTH: mode_t  { return mode_t(0o000002) }
public var S_IXOTH: mode_t  { return mode_t(0o000001) }

public var S_ISUID: mode_t  { return mode_t(0o004000) }
public var S_ISGID: mode_t  { return mode_t(0o002000) }
public var S_ISVTX: mode_t  { return mode_t(0o001000) }
#endif

//===----------------------------------------------------------------------===//
// ioctl.h
//===----------------------------------------------------------------------===//

#if !os(Windows)

public func ioctl(
  _ fd: CInt,
  _ request: UInt,
  _ value: CInt
) -> CInt {
  return _swift_stdlib_ioctl(fd, request, value)
}

public func ioctl(
  _ fd: CInt,
  _ request: UInt,
  _ ptr: UnsafeMutableRawPointer
) -> CInt {
  return _swift_stdlib_ioctlPtr(fd, request, ptr)
}

public func ioctl(
  _ fd: CInt,
  _ request: UInt
) -> CInt {
  return _swift_stdlib_ioctl(fd, request, 0)
}

// !os(Windows)
#endif

//===----------------------------------------------------------------------===//
// signal.h
//===----------------------------------------------------------------------===//

#if os(OpenBSD) || os(FreeBSD)
public var SIG_DFL: sig_t? { return nil }
public var SIG_IGN: sig_t { return unsafeBitCast(1, to: sig_t.self) }
public var SIG_ERR: sig_t { return unsafeBitCast(-1, to: sig_t.self) }
public var SIG_HOLD: sig_t { return unsafeBitCast(3, to: sig_t.self) }
#elseif os(Linux) || os(PS4) || os(Android) || os(Haiku)
#if !canImport(SwiftMusl)
public typealias sighandler_t = __sighandler_t
#endif

public var SIG_DFL: sighandler_t? { return nil }
public var SIG_IGN: sighandler_t {
  return unsafeBitCast(1, to: sighandler_t.self)
}
public var SIG_ERR: sighandler_t {
  return unsafeBitCast(-1, to: sighandler_t.self)
}
public var SIG_HOLD: sighandler_t {
  return unsafeBitCast(2, to: sighandler_t.self)
}
#elseif os(Cygwin)
public typealias sighandler_t = _sig_func_ptr

public var SIG_DFL: sighandler_t? { return nil }
public var SIG_IGN: sighandler_t {
  return unsafeBitCast(1, to: sighandler_t.self)
}
public var SIG_ERR: sighandler_t {
  return unsafeBitCast(-1, to: sighandler_t.self)
}
public var SIG_HOLD: sighandler_t {
  return unsafeBitCast(2, to: sighandler_t.self)
}
#elseif os(Windows)
public var SIG_DFL: _crt_signal_t? { return nil }
public var SIG_IGN: _crt_signal_t {
  return unsafeBitCast(1, to: _crt_signal_t.self)
}
public var SIG_ERR: _crt_signal_t {
  return unsafeBitCast(-1, to: _crt_signal_t.self)
}
#elseif os(WASI)
// No signals support on WASI yet, see https://github.com/WebAssembly/WASI/issues/166.
#else
internal var _ignore = _UnsupportedPlatformError()
#endif

//===----------------------------------------------------------------------===//
// semaphore.h
//===----------------------------------------------------------------------===//

#if !os(Windows) && !os(WASI)

#if os(OpenBSD)
public typealias Semaphore = UnsafeMutablePointer<sem_t?>
#else
public typealias Semaphore = UnsafeMutablePointer<sem_t>
#endif

/// The value returned by `sem_open()` in the case of failure.
public var SEM_FAILED: Semaphore? {
#if os(Linux) || os(FreeBSD) || os(OpenBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku) || os(WASI)
  // The value is ABI.  Value verified to be correct on Glibc.
  return Semaphore(bitPattern: 0)
#else
  _UnsupportedPlatformError()
#endif
}

public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32
) -> Semaphore? {
  return _stdlib_sem_open2(name, oflag)
}

public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> Semaphore? {
  return _stdlib_sem_open4(name, oflag, mode, value)
}

#endif

//===----------------------------------------------------------------------===//
// time.h
//===----------------------------------------------------------------------===//

#if os(Linux)

@available(SwiftStdlib 5.7, *)
extension timespec {
  @available(SwiftStdlib 5.7, *)
  public init(_ duration: Duration) {
    let comps = duration.components
    self.init(tv_sec: Int(comps.seconds),
              tv_nsec: Int(comps.attoseconds / 1_000_000_000))
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration {
  @available(SwiftStdlib 5.7, *)
  public init(_ ts: timespec) {
    self = .seconds(ts.tv_sec) + .nanoseconds(ts.tv_nsec)
  }
}

@available(SwiftStdlib 5.7, *)
extension timeval {
  @available(SwiftStdlib 5.7, *)
  public init(_ duration: Duration) {
    let comps = duration.components
  // Linux platforms define timeval as Int/Int
  self.init(tv_sec: Int(comps.seconds),
              tv_usec: Int(comps.attoseconds / 1_000_000_000_000))
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration {
  @available(SwiftStdlib 5.7, *)
  public init(_ tv: timeval) {
    self = .seconds(tv.tv_sec) + .microseconds(tv.tv_usec)
  }
}

#endif

//===----------------------------------------------------------------------===//
// Misc.
//===----------------------------------------------------------------------===//

// Some platforms don't have `extern char** environ` imported from C.
#if SWIFT_STDLIB_HAS_ENVIRON
#if os(FreeBSD) || os(OpenBSD) || os(PS4)
public var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return _swift_stdlib_getEnviron()
}
#elseif os(Linux) && !canImport(SwiftMusl)
public var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return __environ
}
#endif
#endif // SWIFT_STDLIB_HAS_ENVIRON

#if os(FreeBSD)
@inlinable public func inet_pton(_ af: CInt, _ src: UnsafePointer<CChar>!, _ dst: UnsafeMutableRawPointer!) -> CInt {
  __inet_pton(af, src, dst)
}
#endif
