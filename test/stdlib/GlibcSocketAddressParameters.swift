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

// After we defined '_GNU_SOURCE', the address parameters of these functions
// became typed as '__transparent_union__' union rather than as 'sockaddr *'.
// Swift doesn't support transparent unions, interpreting those as an
// opaque struct, and that made these functions unusable.
//
// Test that every function in the apinotes file does compile when
// a 'sockaddr *' is passed as the parameter.

import Glibc

func acceptConnections(_ fd: CInt) {
  var address = sockaddr()
  var length = socklen_t(MemoryLayout<sockaddr>.size)

  _ = accept(fd, &address, &length)
  _ = accept(fd, nil, nil)
  _ = accept4(fd, &address, &length, 0)
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
