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

var errno : Int32 {
  get {
    return __error().memory
  }
  set {
    __error().memory = newValue
  }
}


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

