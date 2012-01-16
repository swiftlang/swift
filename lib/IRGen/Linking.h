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
#include "llvm/ADT/SmallString.h"
#include "llvm/GlobalValue.h"

namespace llvm {
  class Value;
}

namespace swift {
class NamedDecl;
class ValueDecl;

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

  // These are only meaningful for function entities.
  ExplosionKind Explosion;
  unsigned UncurryLevel;

  static bool isFunction(NamedDecl *decl) {
    return (isa<FuncDecl>(decl) || isa<OneOfElementDecl>(decl));
  }

  LinkEntity() = default;

public:
  static LinkEntity forFunction(ValueDecl *fn, ExplosionKind explosionKind,
                                unsigned uncurryLevel) {
    assert(isFunction(fn));

    LinkEntity entity;
    entity.TheDecl = fn;
    entity.Explosion = explosionKind;
    entity.UncurryLevel = uncurryLevel;
    return entity;
  }
  static LinkEntity forNonFunction(NamedDecl *decl) {
    assert(!isFunction(decl));

    LinkEntity entity;
    entity.TheDecl = decl;
    return entity;
  }

  void mangle(llvm::raw_ostream &out) const;

  NamedDecl *getDecl() const { return TheDecl; }
  ExplosionKind getExplosionKind() const { return Explosion; }
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

#endif
