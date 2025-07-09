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

#include "swift/ABI/MetadataRef.h"
#include "swift/Basic/RelativePointer.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include <cassert>
#include <cstdint>
#include <functional>
#include <iostream>
#include <llvm/ADT/StringRef.h>
#include <ostream>
#include <sstream>
#include <string>

namespace swift {
namespace remote {

/// An abstract address in the remote process's address space.
class RemoteAddress {
public:
  // The default address space, meaning the remote process address space.
  static constexpr uint8_t DefaultAddressSpace = 0;

  explicit RemoteAddress(uint64_t addressData, uint8_t addressSpace)
      : Data(addressData), AddressSpace(addressSpace) {}

  explicit RemoteAddress() {}

  explicit operator bool() const { return Data != 0; }

  bool operator==(const RemoteAddress rhs) const {
    return Data == rhs.Data && AddressSpace == rhs.AddressSpace;
  }

  bool operator!=(const RemoteAddress other) const {
    return !operator==(other);
  }

  bool operator<(const RemoteAddress rhs) const {
    assert(AddressSpace == rhs.AddressSpace &&
           "Comparing remote addresses of different address spaces");
    return Data < rhs.Data;
  }

  bool operator<=(const RemoteAddress rhs) const {
    assert(AddressSpace == rhs.AddressSpace &&
           "Comparing remote addresses of different address spaces");
    return Data <= rhs.Data;
  }

  bool operator>(const RemoteAddress &rhs) const {
    assert(AddressSpace == rhs.AddressSpace &&
           "Comparing remote addresses of different address spaces");
    return Data > rhs.Data;
  }

  bool operator>=(const RemoteAddress &rhs) const { return Data >= rhs.Data; }

  template <typename IntegerType>
  RemoteAddress &operator+=(const IntegerType rhs) {
    Data += rhs;
    return *this;
  }

  template <typename IntegerType>
  RemoteAddress operator+(const IntegerType &rhs) const {
    return RemoteAddress(Data + rhs, getAddressSpace());
  }

  template <typename IntegerType>
  RemoteAddress operator-(const IntegerType &rhs) const {
    return RemoteAddress(Data - rhs, getAddressSpace());
  }

  RemoteAddress operator-(const RemoteAddress &rhs) const {
    if (AddressSpace != rhs.AddressSpace)
      return RemoteAddress();
    return RemoteAddress(Data - rhs.Data, getAddressSpace());
  }

  template <typename IntegerType>
  RemoteAddress operator^(const IntegerType &rhs) const {
    return RemoteAddress(Data ^ rhs, getAddressSpace());
  }

  template <class IntegerType>
  RemoteAddress operator&(IntegerType other) const {
    return RemoteAddress(Data & other, getAddressSpace());
  }

  template <typename IntegerType>
  RemoteAddress &operator&=(const IntegerType rhs) {
    Data &= rhs;
    return *this;
  }

  template <typename IntegerType>
  RemoteAddress &operator|=(const IntegerType rhs) {
    Data |= rhs;
    return *this;
  }

  template <typename IntegerType>
  IntegerType operator>>(const IntegerType rhs) const {
    return (IntegerType)Data >> rhs;
  }

  uint64_t getRawAddress() const { return Data; }

  uint8_t getAddressSpace() const { return AddressSpace; }

  template <class IntegerType>
  RemoteAddress applyRelativeOffset(IntegerType offset) const {
    auto atOffset = detail::applyRelativeOffset((const char *)Data, offset);
    return RemoteAddress(atOffset, getAddressSpace());
  }

  template <typename T, bool Nullable, typename Offset>
  RemoteAddress getRelative(
      const RelativeDirectPointer<T, Nullable, Offset> *relative) const {
    auto ptr = relative->getRelative((void *)Data);
    return RemoteAddress((uint64_t)ptr, getAddressSpace());
  }

  template <class T>
  const T *getLocalPointer() const {
    return reinterpret_cast<const T *>(static_cast<uintptr_t>(Data));
  }

  std::string getDescription() const {
    std::stringstream sstream;
    // FIXME: this should print the address space too, but because Node can't
    // carry the address space yet, comparing the strings produced by this type
    // and a Node that carries an address would produce incorrect results.
    // Revisit this once Node carries the address space.
    sstream << std::hex << Data;
    return sstream.str();
  }

  friend llvm::hash_code hash_value(const RemoteAddress &address) {
    using llvm::hash_value;
    return hash_value(address.Data);
  }

  friend struct std::hash<swift::remote::RemoteAddress>;

private:
  uint64_t Data = 0;
  uint8_t AddressSpace = 0;
};

/// A symbolic relocated absolute pointer value.
class RemoteAbsolutePointer {
  /// The symbol name that the pointer refers to. Empty if only an absolute
  /// address is available.
  std::string Symbol;
  /// The offset from the symbol.
  int64_t Offset = 0;
  /// The resolved remote address.
  RemoteAddress Address = RemoteAddress();

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

template <typename Runtime>
class RemoteTargetProtocolDescriptorRef {
  TargetProtocolDescriptorRef<Runtime> ProtocolRef;
  RemoteAddress address;

public:
  RemoteTargetProtocolDescriptorRef(RemoteAddress address)
      : ProtocolRef(address.getRawAddress()), address(address) {}

  bool isObjC() const { return ProtocolRef.isObjC(); }

  RemoteAddress getObjCProtocol() const {
    auto pointer = ProtocolRef.getObjCProtocol();
    return RemoteAddress(pointer, address.getAddressSpace());
  }

  RemoteAddress getSwiftProtocol() const {
    auto pointer = ProtocolRef.getSwiftProtocol();
    return RemoteAddress(pointer, address.getAddressSpace());
  }
};
} // end namespace remote
} // end namespace swift

namespace std {
template <>
struct hash<swift::remote::RemoteAddress> {
  size_t operator()(const swift::remote::RemoteAddress &address) const {
    return llvm::hash_combine(address.Data, address.AddressSpace);
  }
};
} // namespace std

namespace llvm {
template <>
struct DenseMapInfo<swift::remote::RemoteAddress> {
  static swift::remote::RemoteAddress getEmptyKey() {
    return swift::remote::RemoteAddress(DenseMapInfo<uint64_t>::getEmptyKey(),
                                        0);
  }

  static swift::remote::RemoteAddress getTombstoneKey() {
    return swift::remote::RemoteAddress(
        DenseMapInfo<uint64_t>::getTombstoneKey(), 0);
  }

  static unsigned getHashValue(swift::remote::RemoteAddress address) {
    return std::hash<swift::remote::RemoteAddress>()(address);
  }
  static bool isEqual(swift::remote::RemoteAddress lhs,
                      swift::remote::RemoteAddress rhs) {
    return lhs == rhs;
  }
};
} // namespace llvm

#endif // SWIFT_REMOTE_REMOTEADDRESS_H
