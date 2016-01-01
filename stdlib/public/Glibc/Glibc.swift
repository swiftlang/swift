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

@_exported import SwiftGlibc // Clang module

//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

public var errno: Int32 {
  get {
#if os(FreeBSD)
    return __error().memory
#else
    return __errno_location().memory
#endif
  }
  set(val) {
#if os(FreeBSD)
    return __error().memory = val
#else
    return __errno_location().memory = val
#endif
  }
}

//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

@warn_unused_result
@_silgen_name("_swift_Glibc_open")
func _swift_Glibc_open(path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Glibc_openat")
func _swift_Glibc_openat(
  fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt

@warn_unused_result
public func open(
  path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Glibc_open(path, oflag, 0)
}

@warn_unused_result
public func open(
  path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Glibc_open(path, oflag, mode)
}

@warn_unused_result
public func openat(
  fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt
) -> CInt {
  return _swift_Glibc_openat(fd, path, oflag, 0)
}

@warn_unused_result
public func openat(
  fd: CInt,
  _ path: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t
) -> CInt {
  return _swift_Glibc_openat(fd, path, oflag, mode)
}

@warn_unused_result
@_silgen_name("_swift_Glibc_fcntl")
internal func _swift_Glibc_fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt

@warn_unused_result
@_silgen_name("_swift_Glibc_fcntlPtr")
internal func _swift_Glibc_fcntlPtr(
  fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt
) -> CInt {
  return _swift_Glibc_fcntl(fd, cmd, 0)
}

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ value: CInt
) -> CInt {
  return _swift_Glibc_fcntl(fd, cmd, value)
}

@warn_unused_result
public func fcntl(
  fd: CInt,
  _ cmd: CInt,
  _ ptr: UnsafeMutablePointer<Void>
) -> CInt {
  return _swift_Glibc_fcntlPtr(fd, cmd, ptr)
}

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

//===----------------------------------------------------------------------===//
// signal.h
//===----------------------------------------------------------------------===//

#if os(Linux)
public var SIG_DFL: __sighandler_t? { return nil }
public var SIG_IGN: __sighandler_t {
  return unsafeBitCast(1, __sighandler_t.self)
}
public var SIG_ERR: __sighandler_t {
  return unsafeBitCast(-1, __sighandler_t.self)
}
public var SIG_HOLD: __sighandler_t {
  return unsafeBitCast(2, __sighandler_t.self)
}
#endif

//===----------------------------------------------------------------------===//
// semaphore.h
//===----------------------------------------------------------------------===//

/// The value returned by `sem_open()` in the case of failure.
public var SEM_FAILED: UnsafeMutablePointer<sem_t> {
  // The value is ABI.  Value verified to be correct on Glibc.
  return UnsafeMutablePointer<sem_t>(bitPattern: 0)
}

@warn_unused_result
@_silgen_name("_swift_Glibc_sem_open2")
internal func _swift_Glibc_sem_open2(
  name: UnsafePointer<CChar>,
  _ oflag: CInt
) -> UnsafeMutablePointer<sem_t>

@warn_unused_result
@_silgen_name("_swift_Glibc_sem_open4")
internal func _swift_Glibc_sem_open4(
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
  return _swift_Glibc_sem_open2(name, oflag)
}

@warn_unused_result
public func sem_open(
  name: UnsafePointer<CChar>,
  _ oflag: CInt,
  _ mode: mode_t,
  _ value: CUnsignedInt
) -> UnsafeMutablePointer<sem_t> {
  return _swift_Glibc_sem_open4(name, oflag, mode, value)
}
