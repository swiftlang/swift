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

#include "swift/AST/Decl.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/GlobalValue.h"
#include "IRGen.h"

namespace llvm {
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
  NamedDecl *TheDecl;

  enum class Kind {
    Function, Getter, Setter, Other
  };
  unsigned char TheKind;

  // These are only meaningful for non-Other entities.
  unsigned char Explosion;
  unsigned char UncurryLevel;

  friend struct llvm::DenseMapInfo<LinkEntity>;

  static bool isFunction(NamedDecl *decl) {
    return (isa<FuncDecl>(decl) || isa<OneOfElementDecl>(decl));
  }

  static bool hasGetterSetter(ValueDecl *decl) {
    return (isa<VarDecl>(decl) || isa<SubscriptDecl>(decl));
  }

  Kind getKind() const {
    return Kind(TheKind);
  }

  LinkEntity() = default;

public:
  static LinkEntity forFunction(ValueDecl *fn, ExplosionKind explosionKind,
                                unsigned uncurryLevel) {
    assert(isFunction(fn));

    LinkEntity entity;
    entity.TheDecl = fn;
    entity.TheKind = unsigned(Kind::Function);
    entity.Explosion = unsigned(explosionKind);
    entity.UncurryLevel = uncurryLevel;
    return entity;
  }

  static LinkEntity forNonFunction(NamedDecl *decl) {
    assert(!isFunction(decl));

    LinkEntity entity;
    entity.TheDecl = decl;
    entity.TheKind = unsigned(Kind::Other);
    entity.Explosion = 0;
    entity.UncurryLevel = 0;
    return entity;
  }

  static LinkEntity forGetter(ValueDecl *decl, ExplosionKind explosionKind) {
    assert(hasGetterSetter(decl));
    LinkEntity entity;
    entity.TheDecl = decl;
    entity.TheKind = unsigned(Kind::Getter);
    entity.Explosion = unsigned(explosionKind);
    entity.UncurryLevel = 0;
    return entity;
  }

  static LinkEntity forSetter(ValueDecl *decl, ExplosionKind explosionKind) {
    assert(hasGetterSetter(decl));
    LinkEntity entity;
    entity.TheDecl = decl;
    entity.TheKind = unsigned(Kind::Getter);
    entity.Explosion = unsigned(explosionKind);
    entity.UncurryLevel = 0;
    return entity;
  }

  void mangle(llvm::raw_ostream &out) const;

  NamedDecl *getDecl() const { return TheDecl; }
  ExplosionKind getExplosionKind() const { return ExplosionKind(Explosion); }
  unsigned getUncurryLevel() const { return UncurryLevel; }
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
};

} // end namespace irgen
} // end namespace swift

/// Allow LinkEntity to be used as a key for a DenseMap.
template <> struct llvm::DenseMapInfo<swift::irgen::LinkEntity> {
  typedef swift::irgen::LinkEntity LinkEntity;
  static LinkEntity getEmptyKey() {
    LinkEntity entity;
    entity.TheDecl = nullptr;
    entity.TheKind = 0;
    entity.Explosion = 0;
    entity.UncurryLevel = 0;
    return entity;
  }
  static LinkEntity getTombstoneKey() {
    LinkEntity entity;
    entity.TheDecl = nullptr;
    entity.TheKind = 0;
    entity.Explosion = 0;
    entity.UncurryLevel = 1;
    return entity;
  }
  static unsigned getHashValue(const LinkEntity &entity) {
    return DenseMapInfo<swift::NamedDecl*>::getHashValue(entity.TheDecl)
         ^ ((entity.TheKind << 0) |
            (entity.Explosion << 8) |
            (entity.UncurryLevel << 16));
  }
  static bool isEqual(const LinkEntity &LHS, const LinkEntity &RHS) {
    return LHS.TheDecl == RHS.TheDecl
        && LHS.TheKind == RHS.TheKind
        && LHS.Explosion == RHS.Explosion
        && LHS.UncurryLevel == RHS.UncurryLevel;
  }
};

#endif
