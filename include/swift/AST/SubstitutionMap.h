//===--- SubstitutionMap.h - Swift Substitution Map ASTs --------*- C++ -*-===//
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
// This file defines the SubstitutionMap class.
//
// This is a data structure type describing the mapping of abstract types to
// replacement types, together with associated conformances to use for deriving
// nested types.
//
// Depending on how the SubstitutionMap is constructed, the abstract types are
// either archetypes or interface types. Care must be exercised to only look up
// one or the other.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SUBSTITUTION_MAP_H
#define SWIFT_AST_SUBSTITUTION_MAP_H

#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SubstitutionMap {
  using ParentType = std::pair<CanType, AssociatedTypeDecl *>;

  llvm::DenseMap<TypeBase *, Type> subMap;
  llvm::DenseMap<TypeBase *, ArrayRef<ProtocolConformanceRef>> conformanceMap;
  llvm::DenseMap<TypeBase *, SmallVector<ParentType, 1>> parentMap;

  Optional<ProtocolConformanceRef>
  lookupConformance(ProtocolDecl *proto,
                    ArrayRef<ProtocolConformanceRef> conformances) const;

  template<typename Fn>
  Optional<ProtocolConformanceRef> forEachParent(CanType type, Fn fn) const;

public:
  Optional<ProtocolConformanceRef>
  lookupConformance(CanType type, ProtocolDecl *proto) const;

  const llvm::DenseMap<TypeBase *, Type> &getMap() const {
    return subMap;
  }

  void addSubstitution(CanType type, Type replacement);

  void addConformances(CanType type, ArrayRef<ProtocolConformanceRef> conformances);

  void addParent(CanType type, CanType parent,
                 AssociatedTypeDecl *assocType);

  void removeType(CanType type);
};

} // end namespace swift

#endif
