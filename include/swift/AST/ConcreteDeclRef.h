//===--- ConcreteDeclRef.h - Reference to a concrete decl -------*- C++ -*-===//
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
// This file defines the ConcreteDeclRef class, which provides a reference to
// a declaration that is potentially specialized.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CONCRETEDECLREF_H
#define SWIFT_AST_CONCRETEDECLREF_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Substitution.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Compiler.h"
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
  class SpecializedDeclRef {
    /// The declaration.
    ValueDecl *TheDecl;

    /// The number of substitutions, which are tail allocated.
    unsigned NumSubstitutions;

    SpecializedDeclRef(ValueDecl *decl, ArrayRef<Substitution> substitutions)
      : TheDecl(decl), NumSubstitutions(substitutions.size())
    {
      std::memcpy(reinterpret_cast<Substitution *>(this + 1),
                  substitutions.data(),
                  sizeof(Substitution) * substitutions.size());
    }

  public:
    /// Retrieve the generic declaration.
    ValueDecl *getDecl() const { return TheDecl; }

    /// Retrieve the substitutions.
    ArrayRef<Substitution> getSubstitutions() const {
      return llvm::makeArrayRef(reinterpret_cast<const Substitution *>(this+1),
                                NumSubstitutions);
    }

    /// Allocate a new specialized declaration reference.
    static SpecializedDeclRef *create(ASTContext &ctx, ValueDecl *decl,
                                      ArrayRef<Substitution> substitutions);
  };

  llvm::PointerUnion<ValueDecl *, SpecializedDeclRef *> Data;

public:
  /// Construct a reference to the given value.
  ConcreteDeclRef(ValueDecl *decl) : Data(decl) { }

  /// Construct a reference to the given value, specialized
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
                  ArrayRef<Substitution> substitutions)
    : Data(SpecializedDeclRef::create(ctx, decl, substitutions)) { }

  /// Retrieve the declarations to which this reference refers.
  ValueDecl *getDecl() const {
    if (Data.is<ValueDecl *>())
      return Data.get<ValueDecl *>();

    return Data.get<SpecializedDeclRef *>()->getDecl();
  }

  /// Determine whether this reference specializes the declaration to which
  /// it refers.
  bool isSpecialized() const { return Data.is<SpecializedDeclRef *>(); }

  /// For a specialized reference, return the set of substitutions applied to
  /// the declaration reference.
  ArrayRef<Substitution> getSubstitutions() const {
    assert(isSpecialized() &&
           "Substitutions are only available for specialized references");
    return Data.get<SpecializedDeclRef *>()->getSubstitutions();
  }

  /// Dump a debug representation of this reference.
  void dump(raw_ostream &os);
  void dump() LLVM_ATTRIBUTE_USED;
};

} // end namespace swift


#endif
