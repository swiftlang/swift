//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
public struct DarwinBoolean : Boolean, BooleanLiteralConvertible {
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
@warn_unused_result
public func ==(lhs: DarwinBoolean, rhs: DarwinBoolean) -> Bool {
  return lhs.boolValue == rhs.boolValue
}

@warn_unused_result
public // COMPILER_INTRINSIC
func _convertBoolToDarwinBoolean(_ x: Bool) -> DarwinBoolean {
  return DarwinBoolean(x)
}
@warn_unused_result
public // COMPILER_INTRINSIC
func _convertDarwinBooleanToBool(_ x: DarwinBoolean) -> Bool {
  return Bool(x)
}

// FIXME: We can't make the fully-generic versions @_transparent due to
// rdar://problem/19418937, so here are some @_transparent overloads
// for DarwinBoolean.
@_transparent
@warn_unused_result
public func && <T : Boolean>(
  lhs: T,
  rhs: @autoclosure () -> DarwinBoolean
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

@_transparent
@warn_unused_result
public func || <T : Boolean>(
  lhs: T,
  rhs: @autoclosure () -> DarwinBoolean
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}

#endif

//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

public var errno : Int32 {
  get {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS) || os(FreeBSD)
    return __error().pointee
// FIXME: os(Windows) should be replaced, such as triple(Cygwin)
#elseif os(Android) || os(Windows)
    return __errno().pointee
#else
    return __errno_location().pointee
#endif
  }
  set(val) {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS) || os(FreeBSD)
    return __error().pointee = val
#elseif os(Android) || os(Windows)
    return __errno().pointee = val
#else
    return __errno_location().pointee = val
#endif
  }
}


//===----------------------------------------------------------------------===//
// stdio.h
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS) || os(FreeBSD)
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
#endif


//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

@warn_unused_result
@_silgen_name("_swift_Platform_open")
func _swift_Platform_open(
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Platform_openat")
func _swift_Platform_openat(
  _ fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Platform_open(path, oflag, 0)
}

@warn_unused_result
public func open(
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Platform_open(path, oflag, mode)
}

@warn_unused_result
public func openat(
  _ fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Platform_openat(fd, path, oflag, 0)
}

@warn_unused_result
public func openat(
  _ fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Platform_openat(fd, path, oflag, mode)
}

@warn_unused_result
@_silgen_name("_swift_Platform_fcntl")
internal func _swift_Platform_fcntl(
  _ fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Platform_fcntlPtr")
internal func _swift_Platform_fcntlPtr(
  _ fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt

@warn_unused_result
public func fcntl(
  _ fd: CInt,
  _ cmd: CInt
) -> CInt {
  return _swift_Platform_fcntl(fd, cmd, 0)
}

@warn_unused_result
public func fcntl(
  _ fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt {
  return _swift_Platform_fcntl(fd, cmd, value)
}

@warn_unused_result
public func fcntl(
  _ fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt {
  return _swift_Platform_fcntlPtr(fd, cmd, ptr)
}

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
#elseif os(Linux) || os(FreeBSD) || os(Android) || os(Windows)
#if os(Windows)
// In Cygwin, the below SIG_* have the same value with Linux.
// Verified with libstdc++6 v5.3.0 in Cygwin v2.4.1 64bit.
public typealias sighandler_t = _sig_func_ptr
#else
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
#else
internal var _ignore = _UnsupportedPlatformError()
#endif

//===----------------------------------------------------------------------===//
// semaphore.h
//===----------------------------------------------------------------------===//

/// The value returned by `sem_open()` in the case of failure.
public var SEM_FAILED: UnsafeMutablePointer<sem_t>? {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  // The value is ABI.  Value verified to be correct for OS X, iOS, watchOS, tvOS.
  return UnsafeMutablePointer<sem_t>(bitPattern: -1)
#elseif os(Linux) || os(FreeBSD) || os(Android) || os(Windows)
  // The value is ABI.  Value verified to be correct on Glibc.
  return UnsafeMutablePointer<sem_t>(bitPattern: 0)
#else
  _UnsupportedPlatformError()
#endif
}

@warn_unused_result
@_silgen_name("_swift_Platform_sem_open2")
internal func _swift_Platform_sem_open2(
  _ name: UnsafePointer<CChar>,
  _ oflag: CInt
) -> UnsafeMutablePointer<sem_t>?

@warn_unused_result
@_silgen_name("_swift_Platform_sem_open4")
internal func _swift_Platform_sem_open4(
  _ name: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t>?

@warn_unused_result
public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: CInt
) -> UnsafeMutablePointer<sem_t>? {
  return _swift_Platform_sem_open2(name, oflag)
}

@warn_unused_result
public func sem_open(
  _ name: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t>? {
  return _swift_Platform_sem_open4(name, oflag, mode, value)
}

//===----------------------------------------------------------------------===//
// Misc.
//===----------------------------------------------------------------------===//

// FreeBSD defines extern char **environ differently than Linux.
#if os(FreeBSD)
@warn_unused_result
@_silgen_name("_swift_FreeBSD_getEnv")
func _swift_FreeBSD_getEnv(
) -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>>

public var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return _swift_FreeBSD_getEnv().pointee
}
#endif

