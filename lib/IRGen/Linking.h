//===--- Linking.h - Common declarations for link information ---*- C++ -*-===//
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
// This file defines structures and routines used when creating global
// entities that are placed in the LLVM module, potentially with
// external linkage.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LINKING_H
#define SWIFT_IRGEN_LINKING_H

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/CallingConv.h"
#include "llvm/GlobalValue.h"
#include "IRGen.h"

namespace llvm {
  struct AttributeWithIndex;
  class Value;
}

namespace swift {
namespace irgen {
enum class ExplosionKind : unsigned;
class IRGenModule;

/// A link entity is some sort of named declaration, combined with all
/// the information necessary to distinguish specific implementations
/// of the declaration from each other.
///
/// For example, functions may be exploded or uncurried at different
/// levels, each of which potentially creates a different top-level
/// function.
class LinkEntity {
  /// Either a ValueDecl* or a TypeBase*, depending on Kind.
  void *Pointer;

  /// A hand-rolled bitfield with the following layout:
  unsigned Data;

  enum : unsigned {
    KindShift = 0, KindMask = 0xFF,

    // These fields appear in decl kinds.
    ExplosionLevelShift = 8, ExplosionLevelMask = 0xFF00,
    UncurryLevelShift = 16, UncurryLevelMask = 0xFF0000,

    // This field appears in a type kind.
    ValueWitnessShift = 8, ValueWitnessMask = 0xFF00
  };
#define LINKENTITY_SET_FIELD(field, value) (value << field##Shift)
#define LINKENTITY_GET_FIELD(value, field) ((value & field##Mask) >> field##Shift)

  enum class Kind {
    /// A function.
    /// The pointer is a FuncDecl*.
    Function,

    /// The getter for an entity.
    /// The pointer is a VarDecl* or SubscriptDecl*.
    Getter,

    /// The setter for an entity.
    /// The pointer is a VarDecl* or SubscriptDecl*.
    Setter,

    /// A value witness for a type.
    /// The pointer is a canonical TypeBase*.
    ValueWitness,

    /// The destructor for a class.
    /// The pointer is a ClassDecl*.
    Destructor,

    /// The metadata or metadata template for a class.
    /// The pointer is a ClassDecl*.
    ClassMetadata,

    /// Some other kind of declaration.
    /// The pointer is a Decl*.
    Other
  };
  friend struct llvm::DenseMapInfo<LinkEntity>;

  static bool isFunction(ValueDecl *decl) {
    return (isa<FuncDecl>(decl) || isa<OneOfElementDecl>(decl) ||
            isa<ConstructorDecl>(decl));
  }

  static bool hasGetterSetter(ValueDecl *decl) {
    return (isa<VarDecl>(decl) || isa<SubscriptDecl>(decl));
  }

  Kind getKind() const {
    return Kind(LINKENTITY_GET_FIELD(Data, Kind));
  }

  static bool isDeclKind(Kind k) {
    return !isTypeKind(k);
  }
  static bool isTypeKind(Kind k) {
    return k == Kind::ValueWitness;
  }

  void setForDecl(Kind kind,
                  ValueDecl *decl, ExplosionKind explosionKind,
                  unsigned uncurryLevel) {
    assert(isDeclKind(kind));
    Pointer = decl;
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind))
         | LINKENTITY_SET_FIELD(ExplosionLevel, unsigned(explosionKind))
         | LINKENTITY_SET_FIELD(UncurryLevel, uncurryLevel);
  }

  LinkEntity() = default;

public:
  static LinkEntity forFunction(ValueDecl *fn, ExplosionKind explosionKind,
                                unsigned uncurryLevel) {
    assert(isFunction(fn));

    LinkEntity entity;
    entity.setForDecl(Kind::Function, fn, explosionKind, uncurryLevel);
    return entity;
  }

  static LinkEntity forNonFunction(ValueDecl *decl) {
    assert(!isFunction(decl));

    LinkEntity entity;
    entity.setForDecl(Kind::Other, decl, ExplosionKind(0), 0);
    return entity;
  }

  static LinkEntity forGetter(ValueDecl *decl, ExplosionKind explosionKind) {
    assert(hasGetterSetter(decl));
    LinkEntity entity;
    entity.setForDecl(Kind::Getter, decl, explosionKind, 0);
    return entity;
  }

  static LinkEntity forSetter(ValueDecl *decl, ExplosionKind explosionKind) {
    assert(hasGetterSetter(decl));
    LinkEntity entity;
    entity.setForDecl(Kind::Setter, decl, explosionKind, 0);
    return entity;
  }

  static LinkEntity forDestructor(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::Destructor, decl, ExplosionKind::Minimal, 0);
    return entity;
  }

  static LinkEntity forClassMetadata(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ClassMetadata, decl, ExplosionKind::Minimal, 0);
    return entity;
  }

  static LinkEntity forValueWitness(Type concreteType, ValueWitness witness) {
    assert(concreteType->isCanonical());
    LinkEntity entity;
    entity.Pointer = concreteType.getPointer();
    entity.Data = LINKENTITY_SET_FIELD(Kind, unsigned(Kind::ValueWitness))
                | LINKENTITY_SET_FIELD(ValueWitness, unsigned(witness));
    return entity;
  }

  void mangle(llvm::raw_ostream &out) const;
  bool isLocalLinkage() const;

  ValueDecl *getDecl() const {
    assert(isDeclKind(getKind()));
    return reinterpret_cast<ValueDecl*>(Pointer);
  }
  ExplosionKind getExplosionKind() const {
    assert(isDeclKind(getKind()));
    return ExplosionKind(LINKENTITY_GET_FIELD(Data, ExplosionLevel));
  }
  unsigned getUncurryLevel() const {
    return LINKENTITY_GET_FIELD(Data, UncurryLevel);
  }

  bool isValueWitness() const { return getKind() == Kind::ValueWitness; }
  Type getType() const {
    assert(isTypeKind(getKind()));
    return reinterpret_cast<TypeBase*>(Pointer);
  }
  ValueWitness getValueWitness() const {
    assert(isTypeKind(getKind()));
    return ValueWitness(LINKENTITY_GET_FIELD(Data, ValueWitness));
  }

#undef LINKENTITY_GET_FIELD
#undef LINKENTITY_SET_FIELD
};

/// Encapsulated information about the linkage of an entity.
class LinkInfo {
  LinkInfo() = default;

  llvm::SmallString<32> Name;
  llvm::GlobalValue::LinkageTypes Linkage;
  llvm::GlobalValue::VisibilityTypes Visibility;

public:
  /// Compute linkage information for the given 
  static LinkInfo get(IRGenModule &IGM, const LinkEntity &entity);

  llvm::StringRef getName() const {
    return Name.str();
  }
  llvm::GlobalValue::LinkageTypes getLinkage() const {
    return Linkage;
  }
  llvm::GlobalValue::VisibilityTypes getVisibility() const {
    return Visibility;
  }

  llvm::Function *createFunction(IRGenModule &IGM,
                                 llvm::FunctionType *fnType,
                                 llvm::CallingConv::ID cc,
                                 ArrayRef<llvm::AttributeWithIndex> attrs);


  llvm::GlobalVariable *createVariable(IRGenModule &IGM,
                                       llvm::Type *objectType);
};

} // end namespace irgen
} // end namespace swift

/// Allow LinkEntity to be used as a key for a DenseMap.
template <> struct llvm::DenseMapInfo<swift::irgen::LinkEntity> {
  typedef swift::irgen::LinkEntity LinkEntity;
  static LinkEntity getEmptyKey() {
    LinkEntity entity;
    entity.Pointer = nullptr;
    entity.Data = 0;
    return entity;
  }
  static LinkEntity getTombstoneKey() {
    LinkEntity entity;
    entity.Pointer = nullptr;
    entity.Data = 1;
    return entity;
  }
  static unsigned getHashValue(const LinkEntity &entity) {
    return DenseMapInfo<void*>::getHashValue(entity.Pointer)
         ^ entity.Data;
  }
  static bool isEqual(const LinkEntity &LHS, const LinkEntity &RHS) {
    return LHS.Pointer == RHS.Pointer && LHS.Data == RHS.Data;
  }
};

#endif
