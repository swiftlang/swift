//===--- ConcreteDeclRef.h - Reference to a concrete decl -------*- C++ -*-===//
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
// This file defines the ConcreteDeclRef class, which provides a reference to
// a declaration that is potentially specialized.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CONCRETEDECLREF_H
#define SWIFT_AST_CONCRETEDECLREF_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/SubstitutionList.h"
#include "swift/AST/TypeAlignments.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include <cstring>

namespace swift {

class ASTContext;
class SourceManager;
class ValueDecl;

/// A reference to a concrete representation of a particular declaration,
/// providing substitutions for all type parameters of the original,
/// underlying declaration.
class ConcreteDeclRef {
  /// A specialized declaration reference, which provides substitutions
  /// that fully specialize a generic declaration.
  class SpecializedDeclRef final :
      private llvm::TrailingObjects<SpecializedDeclRef, Substitution> {
    friend TrailingObjects;

    /// The declaration.
    ValueDecl *TheDecl;

    /// The number of substitutions, which are tail allocated.
    unsigned NumSubstitutions;

    SpecializedDeclRef(ValueDecl *decl, SubstitutionList substitutions)
      : TheDecl(decl), NumSubstitutions(substitutions.size())
    {
      std::uninitialized_copy(substitutions.begin(), substitutions.end(),
                              getTrailingObjects<Substitution>());
    }

  public:
    /// Retrieve the generic declaration.
    ValueDecl *getDecl() const { return TheDecl; }

    /// Retrieve the substitutions.
    SubstitutionList getSubstitutions() const {
      return {getTrailingObjects<Substitution>(), NumSubstitutions};
    }
    
    /// Allocate a new specialized declaration reference.
    static SpecializedDeclRef *create(ASTContext &ctx, ValueDecl *decl,
                                      SubstitutionList substitutions);
  };

  llvm::PointerUnion<ValueDecl *, SpecializedDeclRef *> Data;

  friend class llvm::PointerLikeTypeTraits<ConcreteDeclRef>;

public:
  /// Create an empty declaration reference.
  ConcreteDeclRef() : Data() { }

  /// Construct a reference to the given value.
  ConcreteDeclRef(ValueDecl *decl) : Data(decl) { }

  /// Construct a reference to the given value, specialized with the given
  /// substitutions.
  ///
  /// \param ctx The ASTContext in which to allocate the specialized
  /// declaration reference.
  ///
  /// \param decl The declaration to which this reference refers, which will
  /// be specialized by applying the given substitutions.
  ///
  /// \param substitutions The complete set of substitutions to apply to the
  /// given declaration. This array will be copied into the ASTContext by the
  /// constructor.
  ConcreteDeclRef(ASTContext &ctx, ValueDecl *decl,
                  SubstitutionList substitutions)
    : Data(SpecializedDeclRef::create(ctx, decl, substitutions)) { }

  /// Determine whether this declaration reference refers to anything.
  explicit operator bool() const { return !Data.isNull(); }

  /// Retrieve the declarations to which this reference refers.
  ValueDecl *getDecl() const {
    if (Data.is<ValueDecl *>())
      return Data.get<ValueDecl *>();

    return Data.get<SpecializedDeclRef *>()->getDecl();
  }

  /// Retrieve a reference to the declaration this one overrides.
  ConcreteDeclRef
  getOverriddenDecl(ASTContext &ctx, LazyResolver *resolver) const;

  /// Determine whether this reference specializes the declaration to which
  /// it refers.
  bool isSpecialized() const { return Data.is<SpecializedDeclRef *>(); }

  /// For a specialized reference, return the set of substitutions applied to
  /// the declaration reference.
  SubstitutionList getSubstitutions() const {
    if (!isSpecialized())
      return { };
    
    return Data.get<SpecializedDeclRef *>()->getSubstitutions();
  }

  bool operator==(ConcreteDeclRef rhs) const {
    return Data == rhs.Data;
  }
  
  /// Dump a debug representation of this reference.
  void dump(raw_ostream &os);
  void dump() LLVM_ATTRIBUTE_USED;
};

} // end namespace swift

namespace llvm {
  template<> class PointerLikeTypeTraits<swift::ConcreteDeclRef> {
    typedef llvm::PointerUnion<swift::ValueDecl *,
                               swift::ConcreteDeclRef::SpecializedDeclRef *>
      DataPointer;
    typedef PointerLikeTypeTraits<DataPointer> DataTraits;

  public:
    static inline void *
    getAsVoidPointer(swift::ConcreteDeclRef ref) {
      return ref.Data.getOpaqueValue();
    }

    static inline swift::ConcreteDeclRef getFromVoidPointer(void *ptr) {
      swift::ConcreteDeclRef ref;
      ref.Data = DataPointer::getFromOpaqueValue(ptr);
      return ref;
    }

    enum {
      NumLowBitsAvailable = DataTraits::NumLowBitsAvailable
    };
  };
} // end namespace llvm

#endif
