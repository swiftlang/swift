//===--- Substitution.h - Swift Generic Substitution ASTs -------*- C++ -*-===//
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
  class ProtocolConformance;
  
/// DenseMap type used internally by Substitution::subst to track conformances
/// applied to archetypes.
using ArchetypeConformanceMap
  = llvm::DenseMap<ArchetypeType*, ArrayRef<ProtocolConformance*>>;
  
/// Substitution - A substitution into a generic specialization.
class Substitution {
  ArchetypeType * Archetype = nullptr;
  Type Replacement;
  ArrayRef<ProtocolConformance *> Conformance;

public:
  /// FIXME: An archetype that looks like the archetype or dependent generic
  /// parameter type that should be substituted by this substitution, but
  /// which is not guaranteed to map to any particular context. All that is
  /// guaranteed:
  ///
  /// - Archetype will conform to the same protocols as the substituted
  ///   type.
  /// - Archetype will appear at the same point in the generic parameter
  ///   hierarchy as the substituted type; that is, if the substituted type
  ///   is a generic parameter, it will be a primary archetype, or if the
  ///   substituted type is a dependent member type, it will be a nested
  ///   archetype with the same name path--if T0.Foo.Bar is being substituted,
  ///   this will be some archetype X.Foo.Bar.
  /// - If the substituted type represents a Self or associated type of a
  ///   protocol requirement, this Archetype will be that archetype from the
  ///   protocol context.
  ///
  /// You really shouldn't use the value of this field for anything new.
  ArchetypeType *getArchetype() const { return Archetype; }
  
  /// The replacement type.
  Type getReplacement() const { return Replacement; }
  
  /// The protocol conformances for the replacement. These appear in the same
  /// order as Archetype->getConformsTo() for the substituted archetype.
  const ArrayRef<ProtocolConformance *> getConformances() const {
    return Conformance;
  }
  
  Substitution() {}
  
  Substitution(ArchetypeType *Archetype,
               Type Replacement,
               ArrayRef<ProtocolConformance*> Conformance);

  bool operator!=(const Substitution &other) const { return !(*this == other); }
  bool operator==(const Substitution &other) const;
  void print(llvm::raw_ostream &os,
             const PrintOptions &PO = PrintOptions()) const;
  void dump() const;
  
  /// Substitute the replacement and conformance types with the given
  /// substitution vector.
  Substitution subst(Module *module,
                     GenericParamList *context,
                     ArrayRef<Substitution> subs) const;
  
private:
  friend class ProtocolConformance;
  
  Substitution subst(Module *module,
                     ArrayRef<Substitution> subs,
                     TypeSubstitutionMap &subMap,
                     ArchetypeConformanceMap &conformanceMap) const;
};

} // end namespace swift

#endif
