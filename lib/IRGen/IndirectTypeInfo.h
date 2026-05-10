//===--- IndirectTypeInfo.h - Convenience for indirected types --*- C++ -*-===//
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
// This file defines IndirectTypeInfo, which is a convenient abstract
// implementation of TypeInfo for working with types that are always
// passed or returned indirectly.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_INDIRECTTYPEINFO_H
#define SWIFT_IRGEN_INDIRECTTYPEINFO_H

#include "Explosion.h"
#include "TypeInfo.h"
#include "IRGenFunction.h"

namespace swift {
namespace irgen {

/// IndirectTypeInfo - An abstract class designed for use when
/// implementing a type which is always passed indirectly.
///
/// Subclasses must implement the following operations:
///   allocateStack
///   assignWithCopy
///   initializeWithCopy
///   destroy
template <class Derived, class Base>
class IndirectTypeInfo : public Base {
protected:
  template <class... T> IndirectTypeInfo(T &&...args)
    : Base(::std::forward<T>(args)...) {}

  const Derived &asDerived() const {
    return static_cast<const Derived &>(*this);
  }

public:
  void getSchema(ExplosionSchema &schema) const override {
    schema.add(ExplosionSchema::Element::forAggregate(this->getStorageType(),
                                              this->getBestKnownAlignment()));
  }

  void initializeFromParams(IRGenFunction &IGF, Explosion &params, Address dest,
                            SILType T, bool isOutlined) const override {
    Address src = this->getAddressForPointer(params.claimNext());
    asDerived().Derived::initializeWithTake(IGF, dest, src, T, isOutlined,
                                            /*zeroizeIfSensitive=*/ true);
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src, SILType T,
                      bool isOutlined) const override {
    asDerived().Derived::destroy(IGF, dest, T, isOutlined);
    asDerived().Derived::initializeWithTake(IGF, dest, src, T, isOutlined,
                                            /*zeroizeIfSensitive=*/ true);
  }
};

}
}

#endif
