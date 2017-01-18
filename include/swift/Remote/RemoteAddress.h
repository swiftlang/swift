//===--- RemoteAddress.h - Address of remote memory -------------*- C++ -*-===//
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
//
//  This file defines the RemoteAddress type, which abstracts over an
//  address in a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_REMOTEADDRESS_H
#define SWIFT_REMOTE_REMOTEADDRESS_H

#include <cstdint>

namespace swift {
namespace remote {

/// An abstract address in the remote process's address space.
class RemoteAddress {
  uint64_t Data;
public:
  explicit RemoteAddress(const void *localPtr)
    : Data(reinterpret_cast<uintptr_t>(localPtr)) {}

  explicit RemoteAddress(uint64_t addressData) : Data(addressData) {}

  explicit operator bool() const {
    return Data != 0;
  }

  template <class T>
  const T *getLocalPointer() const {
    return reinterpret_cast<const T*>(static_cast<uintptr_t>(Data));
  }

  uint64_t getAddressData() const {
    return Data;
  }
};

} // end namespace remote
} // end namespace swift

#endif // SWIFT_REMOTE_REMOTEADDRESS_H

