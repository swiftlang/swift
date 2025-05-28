//===--- Field.h - Abstract stored field ------------------------*- C++ -*-===//
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
//  This file provides an abstraction for some sort of stored field
//  in a type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FIELD_H
#define SWIFT_IRGEN_FIELD_H

#include "swift/AST/Decl.h"

namespace swift {
class SILType;

namespace irgen {
class IRGenModule;

/// Return the number of fields that will be visited by forEachField,
/// which determines a number of things in the ABI, including the length
/// of the field vector in the type metadata.
///
/// Generally this is the length of the stored properties, but 
/// root default actors have an implicit field for their default-actor
/// storage, and there may also be missing members.
///
/// Even if you're sure that you're in a case where only the stored
/// properties are needed, calling this (and forEachField) instead of
/// directly accessing the stored properties is good practice.
unsigned getNumFields(const NominalTypeDecl *type);

struct Field {
public:
  enum Kind: uintptr_t {
    Var,
    MissingMember,
    DefaultActorStorage,
    NonDefaultDistributedActorStorage,
    FirstArtificial = DefaultActorStorage
  };
  enum : uintptr_t { KindMask = 0x3 };
private:
  uintptr_t declOrKind;
public:
  Field(VarDecl *decl)
      : declOrKind(reinterpret_cast<uintptr_t>(decl) | Var) {
    assert(decl);
    assert(getKind() == Var);
  }
  Field(MissingMemberDecl *decl)
      : declOrKind(reinterpret_cast<uintptr_t>(decl) | MissingMember) {
    assert(decl);
    assert(getKind() == MissingMember);
  }
  Field(Kind kind) : declOrKind(kind) {
    assert(kind >= FirstArtificial);
  }

  Kind getKind() const {
    return Kind(declOrKind & KindMask);
  }
  VarDecl *getVarDecl() const {
    assert(getKind() == Var);
    return reinterpret_cast<VarDecl*>(declOrKind);
  }
  MissingMemberDecl *getMissingMemberDecl() const {
    assert(getKind() == MissingMember);
    return reinterpret_cast<MissingMemberDecl*>(declOrKind & ~KindMask);
  }

  /// Is this a concrete field?  When we're emitting a type, all the
  /// fields should be concrete; non-concrete fields occur only with
  /// imported declarations.
  bool isConcrete() const { return getKind() != MissingMember; }

  /// Return the type of this concrete field.
  SILType getType(IRGenModule &IGM, SILType baseType) const;

  /// Return the interface type of this concrete field.
  Type getInterfaceType(IRGenModule &IGM) const;

  /// Return the nam eof this concrete field.
  llvm::StringRef getName() const;

  bool operator==(Field other) const { return declOrKind == other.declOrKind; }
  bool operator!=(Field other) const { return declOrKind != other.declOrKind; }
};

// Don't export private C++ fields that were imported as private Swift fields.
// The type of a private field might not have all the type witness operations
// that Swift requires, for instance, `std::unique_ptr<IncompleteType>` would
// not have a destructor.
bool isExportableField(Field field);

/// Iterate all the fields of the given struct or class type, including
/// any implicit fields that might be accounted for in
/// getFieldVectorLength.
void forEachField(IRGenModule &IGM, const NominalTypeDecl *typeDecl,
                  llvm::function_ref<void(Field field)> fn);

unsigned countExportableFields(IRGenModule &IGM, const NominalTypeDecl *type);

} // end namespace irgen
} // end namespace swift

#endif
