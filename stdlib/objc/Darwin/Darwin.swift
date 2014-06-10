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

@exported import Darwin // Clang module


//===----------------------------------------------------------------------===//
// sys/errno.h
//===----------------------------------------------------------------------===//

var errno : Int32 {
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

var stdin : UnsafePointer<FILE> {
  get {
    return __stdinp
  }
  set {
    __stdinp = newValue
  }   
}

var stdout : UnsafePointer<FILE> {
  get {
    return __stdoutp
  }
  set {
    __stdoutp = newValue
  }   
}

var stderr : UnsafePointer<FILE> {
  get {
    return __stderrp
  }
  set {
    __stderrp = newValue
  }   
}


//===----------------------------------------------------------------------===//
// sys/stat.h
//===----------------------------------------------------------------------===//

extension stat {
  init() {
    st_dev = 0; st_ino = 0; st_mode = 0; st_nlink = 0; 
    st_uid = 0; st_gid = 0; st_rdev = 0; 
    st_atimespec = timespec(tv_sec:0, tv_nsec:0); 
    st_mtimespec = timespec(tv_sec:0, tv_nsec:0); 
    st_ctimespec = timespec(tv_sec:0, tv_nsec:0); 
    st_birthtimespec = timespec(tv_sec:0, tv_nsec:0); 
    st_size = 0; st_blocks = 0; st_blksize = 0; st_flags = 0; st_gen = 0; 
    st_lspare = 0; st_qspare = (0, 0)
  }
}


//===----------------------------------------------------------------------===//
// fcntl.h
//===----------------------------------------------------------------------===//

@asmname("_swift_Darwin_open") 
func _swift_Darwin_open(path: CString, oflag: CInt, mode: mode_t) -> CInt
@asmname("_swift_Darwin_openat") 
func _swift_Darwin_openat(fd: CInt, path: CString, 
  oflag: CInt, mode: mode_t) -> CInt

func open(path: CString, oflag: CInt) -> CInt {
  return _swift_Darwin_open(path, oflag, 0)
}

func open(path: CString, oflag: CInt, mode: mode_t) -> CInt {
  return _swift_Darwin_open(path, oflag, mode)
}

func openat(fd: CInt, path: CString, oflag: CInt) -> CInt {
  return _swift_Darwin_openat(fd, path, oflag, 0)
}

func openat(fd: CInt, path: CString, oflag: CInt, mode: mode_t) -> CInt {
  return _swift_Darwin_openat(fd, path, oflag, mode)
}


let	S_IFMT   = mode_t(0o170000)
let	S_IFIFO  = mode_t(0o010000)
let	S_IFCHR  = mode_t(0o020000)
let	S_IFDIR  = mode_t(0o040000)
let	S_IFBLK  = mode_t(0o060000)
let	S_IFREG  = mode_t(0o100000)
let	S_IFLNK  = mode_t(0o120000)
let	S_IFSOCK = mode_t(0o140000)
let	S_IFWHT  = mode_t(0o160000)

let	S_IRWXU  = mode_t(0o000700)
let	S_IRUSR  = mode_t(0o000400)
let	S_IWUSR  = mode_t(0o000200)
let	S_IXUSR  = mode_t(0o000100)

let	S_IRWXG  = mode_t(0o000070)
let	S_IRGRP  = mode_t(0o000040)
let	S_IWGRP  = mode_t(0o000020)
let	S_IXGRP  = mode_t(0o000010)

let	S_IRWXO  = mode_t(0o000007)
let	S_IROTH  = mode_t(0o000004)
let	S_IWOTH  = mode_t(0o000002)
let	S_IXOTH  = mode_t(0o000001)

let	S_ISUID  = mode_t(0o004000)
let	S_ISGID  = mode_t(0o002000)
let	S_ISVTX  = mode_t(0o001000)

let	S_ISTXT  = S_ISVTX
let	S_IREAD  = S_IRUSR
let	S_IWRITE = S_IWUSR
let	S_IEXEC  = S_IXUSR


//===----------------------------------------------------------------------===//
// unistd.h
//===----------------------------------------------------------------------===//

@availability(*, unavailable, message="Please use threads or posix_spawn*()")
func fork() -> Int32 {
  errno = ENOSYS
  return -1
}

@availability(*, unavailable, message="Please use threads or posix_spawn*()")
func vfork() -> Int32 {
  errno = ENOSYS
  return -1
}

