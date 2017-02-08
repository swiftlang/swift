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

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
//===----------------------------------------------------------------------===//
// MacTypes.h
//===----------------------------------------------------------------------===//

public var noErr: OSStatus { return 0 }

/// The `Boolean` type declared in MacTypes.h and used throughout Core
/// Foundation.
///
/// The C type is a typedef for `unsigned char`.
@_fixed_layout
public struct DarwinBoolean : ExpressibleByBooleanLiteral {
  var _value: UInt8

  public init(_ value: Bool) {
    self._value = value ? 1 : 0
  }

  /// The value of `self`, expressed as a `Bool`.
  public var boolValue: Bool {
    return _value != 0
  }

  /// Create an instance initialized to `value`.
  @_transparent
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
}

extension DarwinBoolean : CustomReflectable {
  /// Returns a mirror that reflects `self`.
  public var customMirror: Mirror {
    return Mirror(reflecting: boolValue)
  }
}

extension DarwinBoolean : CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    return self.boolValue.description
  }
}

extension DarwinBoolean : Equatable {}
public func ==(lhs: DarwinBoolean, rhs: DarwinBoolean) -> Bool {
  return lhs.boolValue == rhs.boolValue
}

public // COMPILER_INTRINSIC
func _convertBoolToDarwinBoolean(_ x: Bool) -> DarwinBoolean {
  return DarwinBoolean(x)
}
public // COMPILER_INTRINSIC
func _convertDarwinBooleanToBool(_ x: DarwinBoolean) -> Bool {
  return x.boolValue
}

#endif

//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

@_silgen_name("_swift_Platform_getErrno")
func _swift_Platform_getErrno() -> Int32

@_silgen_name("_swift_Platform_setErrno")
func _swift_Platform_setErrno(_: Int32)

public var errno : Int32 {
  get {
    return _swift_Platform_getErrno()
  }
  set(val) {
    return _swift_Platform_setErrno(val)
  }
}


//===----------------------------------------------------------------------===//
// stdio.h
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS) || os(FreeBSD) || os(PS4)
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


//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

#if !os(Windows) || CYGWIN
@_silgen_name("_swift_Platform_open")
func _swift_Platform_open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32
#else
@_silgen_name("_swift_Platform_open")
func _swift_Platform_open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: Int32
) -> Int32
#endif

#if !os(Windows) || CYGWIN
@_silgen_name("_swift_Platform_openat")
func _swift_Platform_openat(
  _ fd: Int32,
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32
#endif

public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32
) -> Int32 {
  return _swift_Platform_open(path, oflag, 0)
}

#if !os(Windows) || CYGWIN
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32 {
  return _swift_Platform_open(path, oflag, mode)
}

public func openat(
  _ fd: Int32,
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32
) -> Int32 {
  return _swift_Platform_openat(fd, path, oflag, 0)
}

public func openat(
  _ fd: Int32,
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t
) -> Int32 {
  return _swift_Platform_openat(fd, path, oflag, mode)
}
#else
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: Int32
) -> Int32 {
  return _swift_Platform_open(path, oflag, mode)
}
#endif

#if !os(Windows) || CYGWIN
@_silgen_name("_swift_Platform_fcntl")
internal func _swift_Platform_fcntl(
  _ fd: Int32,
  _ cmd: Int32,
  _ value: Int32
) -> Int32

@_silgen_name("_swift_Platform_fcntlPtr")
internal func _swift_Platform_fcntlPtr(
  _ fd: Int32,
  _ cmd: Int32,
  _ ptr: UnsafeMutableRawPointer
) -> Int32

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32
) -> Int32 {
  return _swift_Platform_fcntl(fd, cmd, 0)
}

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32,
  _ value: Int32
) -> Int32 {
  return _swift_Platform_fcntl(fd, cmd, value)
}

public func fcntl(
  _ fd: Int32,
  _ cmd: Int32,
  _ ptr: UnsafeMutableRawPointer
) -> Int32 {
  return _swift_Platform_fcntlPtr(fd, cmd, ptr)
}
#endif

#if !os(Windows) || CYGWIN
public var S_IFMT: mode_t   { return mode_t(0o170000) }
public var S_IFIFO: mode_t  { return mode_t(0o010000) }
public var S_IFCHR: mode_t  { return mode_t(0o020000) }
public var S_IFDIR: mode_t  { return mode_t(0o040000) }
public var S_IFBLK: mode_t  { return mode_t(0o060000) }
public var S_IFREG: mode_t  { return mode_t(0o100000) }
public var S_IFLNK: mode_t  { return mode_t(0o120000) }
public var S_IFSOCK: mode_t { return mode_t(0o140000) }
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
public var S_IFWHT: mode_t  { return mode_t(0o160000) }
#endif

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

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
public var S_ISTXT: mode_t  { return S_ISVTX }
public var S_IREAD: mode_t  { return S_IRUSR }
public var S_IWRITE: mode_t { return S_IWUSR }
public var S_IEXEC: mode_t  { return S_IXUSR }
#endif
#else
public var S_IFMT: Int32 { return Int32(0xf000) }

public var S_IFREG: Int32 { return Int32(0x8000) }
public var S_IFDIR: Int32 { return Int32(0x4000) }
public var S_IFCHR: Int32 { return Int32(0x2000) }
public var S_IFIFO: Int32 { return Int32(0x1000) }

public var S_IREAD: Int32  { return Int32(0x0100) }
public var S_IWRITE: Int32 { return Int32(0x0080) }
public var S_IEXEC: Int32  { return Int32(0x0040) }
#endif

//===----------------------------------------------------------------------===//
// ioctl.h
//===----------------------------------------------------------------------===//

#if !os(Windows) || CYGWIN
@_silgen_name("_swift_Platform_ioctl")
internal func _swift_Platform_ioctl(
  _ fd: CInt,
  _ request: UInt,
  _ value: CInt
) -> CInt

@_silgen_name("_swift_Platform_ioctlPtr")
internal func _swift_Platform_ioctlPtr(
  _ fd: CInt,
  _ request: UInt,
  _ ptr: UnsafeMutableRawPointer
) -> CInt

public func ioctl(
  _ fd: CInt,
  _ request: UInt,
  _ value: CInt
) -> CInt {
  return _swift_Platform_ioctl(fd, request, value)
}

public func ioctl(
  _ fd: CInt,
  _ request: UInt,
  _ ptr: UnsafeMutableRawPointer
) -> CInt {
  return _swift_Platform_ioctlPtr(fd, request, ptr)
}

public func ioctl(
  _ fd: CInt,
  _ request: UInt
) -> CInt {
  return _swift_Platform_ioctl(fd, request, 0)
}
#endif

//===----------------------------------------------------------------------===//
// unistd.h
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
@available(*, unavailable, message: "Please use threads or posix_spawn*()")
public func fork() -> Int32 {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message: "Please use threads or posix_spawn*()")
public func vfork() -> Int32 {
  fatalError("unavailable function can't be called")
}
#endif

//===----------------------------------------------------------------------===//
// signal.h
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
public var SIG_DFL: sig_t? { return nil }
public var SIG_IGN: sig_t { return unsafeBitCast(1, to: sig_t.self) }
public var SIG_ERR: sig_t { return unsafeBitCast(-1, to: sig_t.self) }
public var SIG_HOLD: sig_t { return unsafeBitCast(5, to: sig_t.self) }
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
public typealias sighandler_t = __sighandler_t

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
#if CYGWIN
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
#else
public var SIG_DFL: _crt_signal_t? { return nil }
public var SIG_IGN: _crt_signal_t {
  return unsafeBitCast(1, to: _crt_signal_t.self)
}
public var SIG_ERR: _crt_signal_t {
  return unsafeBitCast(-1, to: _crt_signal_t.self)
}
#endif
#else
internal var _ignore = _UnsupportedPlatformError()
#endif

//===----------------------------------------------------------------------===//
// semaphore.h
//===----------------------------------------------------------------------===//

#if !os(Windows) || CYGWIN
/// The value returned by `sem_open()` in the case of failure.
public var SEM_FAILED: UnsafeMutablePointer<sem_t>? {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  // The value is ABI.  Value verified to be correct for OS X, iOS, watchOS, tvOS.
  return UnsafeMutablePointer<sem_t>(bitPattern: -1)
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || CYGWIN
  // The value is ABI.  Value verified to be correct on Glibc.
  return UnsafeMutablePointer<sem_t>(bitPattern: 0)
#else
  _UnsupportedPlatformError()
#endif
}

@_silgen_name("_swift_Platform_sem_open2")
internal func _swift_Platform_sem_open2(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32
) -> UnsafeMutablePointer<sem_t>?

@_silgen_name("_swift_Platform_sem_open4")
internal func _swift_Platform_sem_open4(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t>?

public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32
) -> UnsafeMutablePointer<sem_t>? {
  return _swift_Platform_sem_open2(name, oflag)
}

public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: Int32,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t>? {
  return _swift_Platform_sem_open4(name, oflag, mode, value)
}
#endif

//===----------------------------------------------------------------------===//
// Misc.
//===----------------------------------------------------------------------===//

// FreeBSD defines extern char **environ differently than Linux.
#if os(FreeBSD) || os(PS4)
@_silgen_name("_swift_FreeBSD_getEnv")
func _swift_FreeBSD_getEnv(
) -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>>

public var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return _swift_FreeBSD_getEnv().pointee
}
#endif

