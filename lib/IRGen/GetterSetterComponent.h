//===--- GetterSetterPathComponent.h - Getter/setter lvalues ----*- C++ -*-===//
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
// A helper class for implementing logical path components that use
// getters and setters.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GETTERSETTERPATHCOMPONENT_H
#define SWIFT_IRGEN_GETTERSETTERPATHCOMPONENT_H

#include "LValue.h"

namespace swift {
namespace irgen {

class CallEmission;

/// An abstract implementation of LogicalPathComponent which uses
/// getter/setter function calls.
/// pairs.
class GetterSetterComponent : public LogicalPathComponent {
protected:
  GetterSetterComponent() = default;

  virtual CallEmission prepareStore(IRGenFunction &IGF, Address base,
                                    ExplosionKind bestExplosion,
                                    bool preserve) const = 0;

  virtual CallEmission prepareLoad(IRGenFunction &IGF, Address base,
                                   ExplosionKind bestExplosion,
                                   bool preserve) const = 0;

public:
  void store(IRGenFunction &IGF, const Callee &setter, Address base,
             Explosion &rawValue, const TypeInfo &valueTI,
             bool preserve) const;

  void storeRValue(IRGenFunction &IGF, Expr *rvalue,
                   Address base, bool preserve) const;

  void storeMaterialized(IRGenFunction &IGF, Address temp,
                         Address base, bool preserve) const;

  void storeExplosion(IRGenFunction &IGF, Explosion &value,
                      Address base, bool preserve) const;

  void loadExplosion(IRGenFunction &IGF, Address base,
                     Explosion &exp, bool preserve) const;

  void loadMaterialized(IRGenFunction &IGF, Address base,
                        Address temp, bool preserve) const;

  OwnedAddress loadAndMaterialize(IRGenFunction &IGF,
                                  OnHeap_t onHeap, Address base,
                                  bool preserve) const;
};

#endif
