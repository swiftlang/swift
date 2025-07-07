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

  template<typename IntegerType>
  RemoteAddress& operator+=(const IntegerType& rhs) {
    Data += rhs;
    return *this;
  }

  template<typename IntegerType>
  friend RemoteAddress operator+(RemoteAddress lhs,
                                 const IntegerType& rhs) {
    return lhs += rhs;
  }
};

/// A symbolic relocated absolute pointer value.
class RemoteAbsolutePointer {
  /// The symbol name that the pointer refers to. Empty if only an absolute
  /// address is available.
  std::string Symbol;
  /// The offset from the symbol.
  int64_t Offset = 0;
  /// The resolved remote address.
  RemoteAddress Address = RemoteAddress{(uint64_t)0};

public:
  RemoteAbsolutePointer() = default;
  RemoteAbsolutePointer(std::nullptr_t) : RemoteAbsolutePointer() {}

  RemoteAbsolutePointer(llvm::StringRef Symbol, int64_t Offset,
                        RemoteAddress Address)
      : Symbol(Symbol), Offset(Offset), Address(Address) {}
  RemoteAbsolutePointer(RemoteAddress Address) : Address(Address) {}

  llvm::StringRef getSymbol() const { return Symbol; }
  int64_t getOffset() const { return Offset; }

  RemoteAddress getResolvedAddress() const { return Address; }

  explicit operator bool() const {
    return Address || !Symbol.empty();
  }
};

} // end namespace remote
} // end namespace swift

#endif // SWIFT_REMOTE_REMOTEADDRESS_H

