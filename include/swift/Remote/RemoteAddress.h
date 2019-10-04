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
#include <string>
#include <llvm/ADT/StringRef.h>
#include <cassert>

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

/// A symbolic relocated absolute pointer value.
class RemoteAbsolutePointer {
  /// The symbol name that the pointer refers to. Empty if the value is absolute.
  std::string Symbol;
  /// The offset from the symbol, or the resolved remote address if \c Symbol is empty.
  int64_t Offset;

public:
  RemoteAbsolutePointer()
    : Symbol(), Offset(0)
  {}
  
  RemoteAbsolutePointer(std::nullptr_t)
    : RemoteAbsolutePointer()
  {}
  
  RemoteAbsolutePointer(llvm::StringRef Symbol, int64_t Offset)
    : Symbol(Symbol), Offset(Offset)
  {}
  
  bool isResolved() const { return Symbol.empty(); }
  llvm::StringRef getSymbol() const { return Symbol; }
  int64_t getOffset() const { return Offset; }
  
  RemoteAddress getResolvedAddress() const {
    assert(isResolved());
    return RemoteAddress(Offset);
  }
  
  explicit operator bool() const {
    return Offset != 0 || !Symbol.empty();
  }
};

} // end namespace remote
} // end namespace swift

#endif // SWIFT_REMOTE_REMOTEADDRESS_H

