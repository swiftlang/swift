//===--- GetterSetter.h - Getter/setter lvalue components -------*- C++ -*-===//
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
// Helper classes for implementing logical path components that use
// getters and setters.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GETTERSETTER_H
#define SWIFT_IRGEN_GETTERSETTER_H

#include "LValue.h"

namespace swift {
namespace irgen {

class CallEmission;

/// An abstract implementation of LogicalPathComponent which uses
/// getter/setter function calls.
/// pairs.
class GetterSetter : public LogicalPathComponent {
protected:
  GetterSetter() = default;

  /// Begin a call to the getter.
  virtual CallEmission prepareGetter(IRGenFunction &IGF, Address base,
                                     ExplosionKind bestExplosion,
                                     ShouldPreserveValues preserve) const = 0;

  /// Begin a call to the setter.
  virtual CallEmission prepareSetter(IRGenFunction &IGF, Address base,
                                     ExplosionKind bestExplosion,
                                     ShouldPreserveValues preserve) const = 0;

public:
  void store(IRGenFunction &IGF, const Callee &setter, Address base,
             Explosion &rawValue, const TypeInfo &valueTI,
             bool preserve) const;

  void storeRValue(IRGenFunction &IGF, Expr *rvalue,
                   Address base, ShouldPreserveValues preserve) const;

  void storeMaterialized(IRGenFunction &IGF, Address temp,
                         Address base, ShouldPreserveValues preserve) const;

  void storeExplosion(IRGenFunction &IGF, Explosion &value,
                      Address base, ShouldPreserveValues preserve) const;

  void loadExplosion(IRGenFunction &IGF, Address base,
                     Explosion &exp, ShouldPreserveValues preserve) const;

  void loadMaterialized(IRGenFunction &IGF, Address base,
                        Address temp, ShouldPreserveValues preserve) const;

  OwnedAddress loadAndMaterialize(IRGenFunction &IGF,
                                  OnHeap_t onHeap, Address base,
                                  ShouldPreserveValues preserve) const;
};

/// A helper class for defining a member component
class MemberGetterSetterBase : public GetterSetter {
  unsigned NumBaseValues : 12;
  unsigned NumIndexValues : 12;
  unsigned ThisSize : 8; // Dynamic sizeof this
  ValueDecl *Target;

  ManagedValue *getBufferStart() {
    return reinterpret_cast<ManagedValue*>(
                                   reinterpret_cast<char*>(this) + ThisSize);
  }
  const ManagedValue *getBufferStart() const {
    return reinterpret_cast<const ManagedValue*>(
                             reinterpret_cast<const char*>(this) + ThisSize);
  }

  ManagedValue *getBaseValuesBuffer() {
    return getBufferStart();
  }
  const ManagedValue *getBaseValuesBuffer() const {
    return getBufferStart();
  }
  ManagedValue *getIndexValuesBuffer() {
    return getBufferStart() + NumBaseValues;
  }
  const ManagedValue *getIndexValuesBuffer() const {
    return getBufferStart() + NumBaseValues;
  }

protected:
  MemberGetterSetterBase(ValueDecl *target,
                         unsigned numBaseValues,
                         unsigned numIndexValues,
                         size_t thisSize)
    : GetterSetter(),
      NumBaseValues(numBaseValues), NumIndexValues(numIndexValues),
      ThisSize(thisSize), Target(target) {
    assert(thisSize >= sizeof(MemberGetterSetterBase));
  }

  void initBaseValues(Explosion &values);
  void initIndexValues(Explosion &values);

  /// Add the base values to the given explosion.
  void addBaseValues(IRGenFunction &IGF, Explosion &out,
                     ShouldPreserveValues preserve,
                     CanType origBaseType, CanType substBaseType,
                     ArrayRef<Substitution> subs) const;

  /// Add the index values to the given explosion.
  void addIndexValues(IRGenFunction &IGF, Explosion &out,
                      ShouldPreserveValues preserve,
                      CanType origIndexType, CanType substIndexType,
                      ArrayRef<Substitution> subs) const;

public:
  static size_t extra_storage_size(ValueDecl *target, Explosion &args) {
    assert(args.getKind() == ExplosionKind::Maximal);
    return args.size() * sizeof(ManagedValue);
  }

  static size_t extra_storage_size(ValueDecl *target, Explosion &args1,
                                   Explosion &args2) {
    assert(args1.getKind() == ExplosionKind::Maximal);
    assert(args2.getKind() == ExplosionKind::Maximal);
    return (args1.size() + args2.size()) * sizeof(ManagedValue);
  }

  ValueDecl *getTarget() const { return Target; }
};

template <class Impl, class Base = MemberGetterSetterBase>
class MemberGetterSetter : public Base {
protected:
  MemberGetterSetter(SubscriptDecl *target,
                     unsigned numBaseValues, unsigned numIndexValues)
    : Base(target, numBaseValues, numIndexValues, sizeof(Impl)) {
  }

  MemberGetterSetter(VarDecl *target, unsigned numBaseValues)
    : Base(target, numBaseValues, 0, sizeof(Impl)) {
  }
};

}
}

#endif
