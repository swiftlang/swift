//===----------------------------------------------------------------------===//
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

// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=linux-gnu

// Defining '_GNU_SOURCE' changes the declared type of some Glibc parameters,
// and the spelling of some members. Ensure that the affected declarations
// accept the types and names expected by existing callers, to avoid source
// breaks as much as possible.
//
// This is a compatibility test; it is expected to succeed whether or not
// '_GNU_SOURCE' was defined when building the Glibc module.

import Glibc

// MARK: - Address parameters
//
// These became typed as a '__transparent_union__' union rather than as
// 'sockaddr *'. The parameter is rewritten as 'sockaddr *' in apinotes.

func acceptConnections(_ fd: CInt) {
  var address = sockaddr()
  var length = socklen_t(MemoryLayout<sockaddr>.size)

  _ = accept(fd, &address, &length)
  _ = accept(fd, nil, nil)
}

func nameSockets(_ fd: CInt) {
  var address = sockaddr()
  var length = socklen_t(MemoryLayout<sockaddr>.size)

  _ = getsockname(fd, &address, &length)
  _ = getpeername(fd, &address, &length)
}

func connectSockets(_ fd: CInt) {
  var address = sockaddr()
  let length = socklen_t(MemoryLayout<sockaddr>.size)

  _ = bind(fd, &address, length)
  _ = connect(fd, &address, length)
}

func transfer(_ fd: CInt) {
  var address = sockaddr()
  var addressLength = socklen_t(MemoryLayout<sockaddr>.size)
  var buffer = [UInt8](repeating: 0, count: 8)

  _ = sendto(fd, &buffer, buffer.count, 0, &address, addressLength)
  _ = recvfrom(fd, &buffer, buffer.count, 0, &address, &addressLength)
  _ = recvfrom(fd, &buffer, buffer.count, 0, nil, nil)
}

// MARK: - Enumerated parameters
//
// The following functions's first parameter is usually declared as 'int',
// but under '__USE_GNU' it becomes an enumeration. The Glibc overlay adds
// 'CInt' overloads for compatibility.

func resourceLimits() {
  var limit = rlimit(rlim_cur: 1, rlim_max: 1)

  _ = getrlimit(CInt(RLIMIT_CORE.rawValue), &limit)
  _ = setrlimit(CInt(RLIMIT_CORE.rawValue), &limit)
}

func resourceUsage() {
  var usage = rusage()

  _ = getrusage(CInt(RUSAGE_SELF.rawValue), &usage)
  _ = getrusage(CInt(RUSAGE_CHILDREN.rawValue), &usage)
}

func schedulingPriority() {
  _ = getpriority(CInt(PRIO_PROCESS.rawValue), 0)
  _ = setpriority(CInt(PRIO_PROCESS.rawValue), 0, 0)
}

func intervalTimers() {
  var timer = itimerval()

  _ = getitimer(CInt(ITIMER_REAL.rawValue), &timer)
  _ = setitimer(CInt(ITIMER_REAL.rawValue), &timer, nil)
}

func typedefSpelling(_ pid: CInt) {
  var limit = rlimit(rlim_cur: 1, rlim_max: 1)

  _ = setrlimit(__rlimit_resource_t(RLIMIT_CORE.rawValue), &limit)
  _ = setpriority(__priority_which_t(PRIO_PROCESS.rawValue), UInt32(pid), 0)
}

func integerLiterals() {
  var usage = rusage()

  _ = getrusage(0, &usage)
}

// Test the imported signatures, so that a partially applied reference remains
// compatible. These function types are from before we defined '_GNU_SOURCE'.
func signatures() {
  let _: (CInt, UnsafeMutablePointer<rlimit>) -> CInt = getrlimit
  let _: (CInt, UnsafePointer<rlimit>) -> CInt = setrlimit
  let _: (CInt, UnsafeMutablePointer<rusage>?) -> CInt = getrusage
  let _: (CInt, id_t) -> CInt = getpriority
  let _: (CInt, id_t, CInt) -> CInt = setpriority
  let _: (CInt, UnsafeMutablePointer<itimerval>?) -> CInt = getitimer
  let _: (CInt, UnsafePointer<itimerval>?, UnsafeMutablePointer<itimerval>?) -> CInt = setitimer
}

// MARK: - Structure members
//
// Test the presence of struct members renamed by _GNU_SOURCE.

func fileDescriptorSets() {
  var set = fd_set()

  let _: Int = set.__fds_bits.3
  set.__fds_bits.3 = 42
}
