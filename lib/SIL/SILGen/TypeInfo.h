//===--- TypeInfo.h - Type information relevant to SILGen -------*- C++ -*-===//
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

#ifndef SILGen_TypeInfo_h
#define SILGen_TypeInfo_h

#include "SILGen.h"
#include "Explosion.h"

namespace swift {
namespace Lowering {

class LLVM_LIBRARY_VISIBILITY TypeInfo {
  friend class TypeInfoVisitor;
  
  bool addressOnly : 1;
  bool trivial : 1;

  TypeInfo(bool addressOnly, bool trivial)
    : addressOnly(addressOnly), trivial(trivial)
  {
    assert(!(addressOnly && trivial) &&
           "type cannot be both address-only and trivial");
  }
  
public:
  TypeInfo() = default;
  
  /// isAddressOnly - Returns true if the type is an address-only type. A type
  /// is address-only if it is a resilient value type, or if it is a fragile
  /// value type with a resilient member. In either case, the full layout of
  /// values of the type is unavailable to the compiler.
  bool isAddressOnly() const { return addressOnly; }
  /// isLoadable - Returns true if the type is loadable, in other words, its
  /// full layout is available to the compiler. This is the inverse of
  /// isAddressOnly.
  bool isLoadable() const { return !addressOnly; }
  
  /// isTrivial - Returns true if the type is trivial, meaning it is a loadable
  /// value type with no reference type members that require releasing.
  bool isTrivial() const { return trivial; }
  
  /// get - Returns a TypeInfo value with information about a type.
  static TypeInfo get(Type t);
};
  
} // namespace Lowering
} // namespace swift

#endif
