//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Darwin // Clang module


//===----------------------------------------------------------------------===//
// MacTypes.h
//===----------------------------------------------------------------------===//

public var noErr: OSStatus { return 0 }

/// The `Boolean` type declared in MacTypes.h and used throughout Core
/// Foundation.
///
/// The C type is a typedef for `unsigned char`.
public struct DarwinBoolean : BooleanType, BooleanLiteralConvertible {
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

extension DarwinBoolean : _Reflectable {
  /// Returns a mirror that reflects `self`.
  public func _getMirror() -> _MirrorType {
    return _reflect(boolValue)
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
func _convertBoolToDarwinBoolean(x: Bool) -> DarwinBoolean {
  return DarwinBoolean(x)
}
@warn_unused_result
public // COMPILER_INTRINSIC
func _convertDarwinBooleanToBool(x: DarwinBoolean) -> Bool {
  return Bool(x)
}

// FIXME: We can't make the fully-generic versions @_transparent due to
// rdar://problem/19418937, so here are some @_transparent overloads
// for DarwinBoolean.
@_transparent
@warn_unused_result
public func && <T : BooleanType>(
  lhs: T,
  @autoclosure rhs: () -> DarwinBoolean
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

@_transparent
@warn_unused_result
public func || <T : BooleanType>(
  lhs: T,
  @autoclosure rhs: () -> DarwinBoolean
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}


//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

public var errno : Int32 {
  get {
    return __error().memory
  }
  set {
    __error().memory = newValue
  }
}


//===----------------------------------------------------------------------===//
// stdio.h
//===----------------------------------------------------------------------===//

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


//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

@warn_unused_result
@_silgen_name("_swift_Darwin_open") 
func _swift_Darwin_open(
  path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Darwin_openat")
func _swift_Darwin_openat(fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
public func open(
  path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Darwin_open(path, oflag, 0)
}

@warn_unused_result
public func open(
  path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Darwin_open(path, oflag, mode)
}

@warn_unused_result
public func openat(
  fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Darwin_openat(fd, path, oflag, 0)
}

@warn_unused_result
public func openat(
  fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Darwin_openat(fd, path, oflag, mode)
}

@warn_unused_result
@_silgen_name("_swift_Darwin_fcntl")
internal func _swift_Darwin_fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Darwin_fcntlPtr")
internal func _swift_Darwin_fcntlPtr(
  fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt
) -> CInt {
  return _swift_Darwin_fcntl(fd, cmd, 0)
}

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt {
  return _swift_Darwin_fcntl(fd, cmd, value)
}

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt {
  return _swift_Darwin_fcntlPtr(fd, cmd, ptr)
}

public var S_IFMT: mode_t   { return mode_t(0o170000) }
public var S_IFIFO: mode_t  { return mode_t(0o010000) }
public var S_IFCHR: mode_t  { return mode_t(0o020000) }
public var S_IFDIR: mode_t  { return mode_t(0o040000) }
public var S_IFBLK: mode_t  { return mode_t(0o060000) }
public var S_IFREG: mode_t  { return mode_t(0o100000) }
public var S_IFLNK: mode_t  { return mode_t(0o120000) }
public var S_IFSOCK: mode_t { return mode_t(0o140000) }
public var S_IFWHT: mode_t  { return mode_t(0o160000) }

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

public var S_ISTXT: mode_t  { return S_ISVTX }
public var S_IREAD: mode_t  { return S_IRUSR }
public var S_IWRITE: mode_t { return S_IWUSR }
public var S_IEXEC: mode_t  { return S_IXUSR }

//===----------------------------------------------------------------------===//
// unistd.h
//===----------------------------------------------------------------------===//

@available(*, unavailable, message="Please use threads or posix_spawn*()")
public func fork() -> Int32 {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="Please use threads or posix_spawn*()")
public func vfork() -> Int32 {
  fatalError("unavailable function can't be called")
}

//===----------------------------------------------------------------------===//
// signal.h
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
public var SIG_DFL: sig_t? { return nil }
public var SIG_IGN: sig_t { return unsafeBitCast(1, sig_t.self) }
public var SIG_ERR: sig_t { return unsafeBitCast(-1, sig_t.self) }
public var SIG_HOLD: sig_t { return unsafeBitCast(5, sig_t.self) }
#else
internal var _ignore = _UnsupportedPlatformError()
#endif

//===----------------------------------------------------------------------===//
// semaphore.h
//===----------------------------------------------------------------------===//

/// The value returned by `sem_open()` in the case of failure.
public var SEM_FAILED: UnsafeMutablePointer<sem_t> {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  // The value is ABI.  Value verified to be correct for OS X, iOS, watchOS, tvOS.
  return UnsafeMutablePointer<sem_t>(bitPattern: -1)
#else
  _UnsupportedPlatformError()
#endif
}

@warn_unused_result
@_silgen_name("_swift_Darwin_sem_open2")
internal func _swift_Darwin_sem_open2(
  name: UnsafePointer<CChar>,
  _ oflag: CInt
) -> UnsafeMutablePointer<sem_t>

@warn_unused_result
@_silgen_name("_swift_Darwin_sem_open4")
internal func _swift_Darwin_sem_open4(
  name: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t>

@warn_unused_result
public func sem_open(
  name: UnsafePointer<CChar>,
  _ oflag: CInt
) -> UnsafeMutablePointer<sem_t> {
  return _swift_Darwin_sem_open2(name, oflag)
}

@warn_unused_result
public func sem_open(
  name: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t> {
  return _swift_Darwin_sem_open4(name, oflag, mode, value)
}

