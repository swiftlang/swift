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
  
  /// Substitute the replacement and conformance types with the given
  /// substitution vector.
  Substitution subst(ModuleDecl *module,
                     GenericParamList *context,
                     ArrayRef<Substitution> subs) const;
  
private:
  friend class ProtocolConformance;
  
  Substitution subst(ModuleDecl *module,
                     ArrayRef<Substitution> subs,
                     TypeSubstitutionMap &subMap,
                     ArchetypeConformanceMap &conformanceMap) const;
};

/// An iterator over a list of archetypes and the substitutions
/// applied to them.
class SubstitutionIterator {
  // TODO: this should use dependent types when getConformsTo() becomes
  // efficient there.
  ArrayRef<ArchetypeType*> Archetypes;
  ArrayRef<Substitution> Subs;

public:
  SubstitutionIterator() = default;
  explicit SubstitutionIterator(GenericParamList *params,
                                ArrayRef<Substitution> subs);

  struct iterator {
    ArchetypeType * const *NextArch = nullptr;
    const Substitution *NextSub = nullptr;

    iterator() = default;
    iterator(ArchetypeType * const *nextArch, const Substitution *nextSub)
      : NextArch(nextArch), NextSub(nextSub) {}

    iterator &operator++() {
      ++NextArch;
      ++NextSub;
      return *this;
    }

    iterator operator++(int) {
      iterator copy = *this;
      ++*this;
      return copy;
    }

    std::pair<ArchetypeType*,Substitution> operator*() const {
      return { *NextArch, *NextSub };
    }

    bool operator==(const iterator &other) const {
      assert((NextSub == other.NextSub) == (NextArch == other.NextArch));
      return NextSub == other.NextSub;
    }
    bool operator!=(const iterator &other) const {
      return !(*this == other);
    }
  };

  ArrayRef<Substitution> getSubstitutions() const { return Subs; }

  bool empty() const { return Archetypes.empty(); }

  iterator begin() const { return { Archetypes.begin(), Subs.begin() }; }
  iterator end() const { return { Archetypes.end(), Subs.end() }; }
};

void dump(const ArrayRef<Substitution> &subs);

} // end namespace swift

#endif
