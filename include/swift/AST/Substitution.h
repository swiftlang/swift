//===--- Substitution.h - Swift Generic Substitution ASTs -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Substitution class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SUBSTITUTION_H
#define SWIFT_AST_SUBSTITUTION_H

#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ArchetypeType;
  class GenericEnvironment;
  class ProtocolConformanceRef;
  
/// DenseMap type used internally by Substitution::subst to track conformances
/// applied to archetypes.
using ArchetypeConformanceMap
  = llvm::DenseMap<ArchetypeType*, ArrayRef<ProtocolConformanceRef>>;

/// Substitution - A substitution into a generic specialization.
class Substitution {
  Type Replacement;
  ArrayRef<ProtocolConformanceRef> Conformance;

public:
  /// The replacement type.
  Type getReplacement() const { return Replacement; }
  
  /// The protocol conformances for the replacement. These appear in the same
  /// order as Archetype->getConformsTo() for the substituted archetype.
  const ArrayRef<ProtocolConformanceRef> getConformances() const {
    return Conformance;
  }
  
  Substitution() {}
  
  Substitution(Type Replacement, ArrayRef<ProtocolConformanceRef> Conformance);

  bool operator!=(const Substitution &other) const { return !(*this == other); }
  bool operator==(const Substitution &other) const;
  void print(llvm::raw_ostream &os,
             const PrintOptions &PO = PrintOptions()) const;
  void dump() const;
  void dump(llvm::raw_ostream &os, unsigned indent = 0) const;
  
  /// Apply a substitution to this substitution's replacement type and
  /// conformances.
  ///
  /// Our replacement type must be written in terms of the context
  /// archetypes of 'env', which in turn must be derived from the
  /// generic requirements of 'sig'.
  Substitution subst(ModuleDecl *module,
                     GenericSignature *sig,
                     GenericEnvironment *env,
                     ArrayRef<Substitution> subs) const;

  Substitution subst(ModuleDecl *module,
                     TypeSubstitutionMap &subMap,
                     ArchetypeConformanceMap &conformanceMap) const;

private:
  friend class ProtocolConformance;
};

void dump(const ArrayRef<Substitution> &subs);

} // end namespace swift

#endif
