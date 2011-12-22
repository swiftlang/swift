//===--- Address.h - Address Representation ---------------------*- C++ -*-===//
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
//
// A structure for holding the address of an object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_ADDRESS_H
#define SWIFT_IRGEN_ADDRESS_H

#include "IRGen.h"

namespace swift {
namespace irgen {

/// The address of an object in memory.
class Address {
  llvm::Value *Addr;
  Alignment Align;

public:
  Address() : Addr(nullptr) {}
  Address(llvm::Value *addr, Alignment align) : Addr(addr), Align(align) {
    assert(addr != nullptr && "building an invalid address");
  }

  bool isValid() const { return Addr != nullptr; }

  llvm::Value *getAddress() const {
    return Addr;
  }

  Alignment getAlignment() const {
    return Align;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
