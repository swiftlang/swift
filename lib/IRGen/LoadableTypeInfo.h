//===--- LoadableTypeInfo.h - Supplement for loadable types -----*- C++ -*-===//
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
// This file defines LoadableTypeInfo, which supplements the TypeInfo
// interface for types that support being loaded and stored as
// Explosions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LOADABLETYPEINFO_H
#define SWIFT_IRGEN_LOADABLETYPEINFO_H

#include "FixedTypeInfo.h"

namespace swift {
namespace irgen {

/// LoadableTypeInfo - A refinement of FixedTypeInfo designed for use
/// when implementing a type that can be loaded into an explosion.
/// Types that are not loadable are called address-only; this is the
/// same concept as exists in SIL.
///
/// The semantics of most of these operations are specified as if an
/// exploded value were a normal object that is merely not located in
/// memory.
class LoadableTypeInfo : public FixedTypeInfo {
protected:
  LoadableTypeInfo(llvm::Type *type, Size size, llvm::BitVector spareBits,
                   Alignment align,
                   IsPOD_t pod, SpecialTypeInfoKind stik = STIK_Loadable)
      : FixedTypeInfo(type, size, spareBits, align, pod, stik) {
    assert(isLoadable());
  }

public:
  // This is useful for metaprogramming.
  static bool isLoadable() { return true; }

  /// Return the number of elements in an explosion of this type.
  virtual unsigned getExplosionSize(Mangle::ExplosionKind kind) const = 0;

  /// Load an explosion of values from an address as if copy-initializing
  /// a set of registers.
  virtual void loadAsCopy(IRGenFunction &IGF, Address addr,
                          Explosion &explosion) const = 0;

  /// Load an explosion of values from an address as if
  /// take-initializing a set of registers.
  virtual void loadAsTake(IRGenFunction &IGF, Address addr,
                          Explosion &explosion) const = 0;

  /// Assign a set of exploded values into an address.  The values are
  /// consumed out of the explosion.
  virtual void assign(IRGenFunction &IGF, Explosion &explosion,
                      Address addr) const = 0;

  /// Initialize an address by consuming values out of an explosion.
  virtual void initialize(IRGenFunction &IGF, Explosion &explosion,
                          Address addr) const = 0;


  // We can give this a reasonable default implementation.
  void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const override;

  /// Consume a bunch of values which have exploded at one explosion
  /// level and produce them at another.
  ///
  /// Essentially, this is like take-initializing the new explosion.
  virtual void reexplode(IRGenFunction &IGF, Explosion &sourceExplosion,
                         Explosion &targetExplosion) const = 0;

  /// Pack the source explosion into a union payload.
  virtual llvm::Value *packUnionPayload(IRGenFunction &IGF,
                                        Explosion &sourceExplosion,
                                        unsigned bitWidth) const = 0;
  
  /// Unpack a union payload containing a valid value of the type into the
  /// destination explosion.
  virtual void unpackUnionPayload(IRGenFunction &IGF,
                                  llvm::Value *payload,
                                  Explosion &targetExplosion) const = 0;

  static bool classof(const LoadableTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) { return type->isLoadable(); }
};

}
}

#endif
