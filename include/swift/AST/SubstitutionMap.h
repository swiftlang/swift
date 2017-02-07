//===--- SubstitutionMap.h - Swift Substitution Map ASTs --------*- C++ -*-===//
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

class SubstitutableType;

template<class Type> class CanTypeWrapper;
typedef CanTypeWrapper<SubstitutableType> CanSubstitutableType;

class SubstitutionMap {
  using ParentType = std::pair<CanType, AssociatedTypeDecl *>;

  llvm::DenseMap<SubstitutableType *, Type> subMap;
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

  const llvm::DenseMap<SubstitutableType *, Type> &getMap() const {
    return subMap;
  }

  /// Retrieve the conformances for the given type.
  ArrayRef<ProtocolConformanceRef> getConformances(CanType type) const;

  void addSubstitution(CanSubstitutableType type, Type replacement);

  void addConformances(CanType type, ArrayRef<ProtocolConformanceRef> conformances);

  void addParent(CanType type, CanType parent,
                 AssociatedTypeDecl *assocType);
  
  bool empty() const {
    return subMap.empty();
  }

  /// Given that 'derivedDecl' is an override of 'baseDecl' in a subclass,
  /// and 'derivedSubs' is a set of substitutions written in terms of the
  /// generic signature of 'derivedDecl', produce a set of substitutions
  /// written in terms of the generic signature of 'baseDecl'.
  static SubstitutionMap
  getOverrideSubstitutions(const ValueDecl *baseDecl,
                           const ValueDecl *derivedDecl,
                           Optional<SubstitutionMap> derivedSubs,
                           LazyResolver *resolver);

  /// Variant of the above for when we have the generic signatures but not
  /// the decls for 'derived' and 'base'.
  static SubstitutionMap
  getOverrideSubstitutions(const ClassDecl *baseClass,
                           const ClassDecl *derivedClass,
                           GenericSignature *baseSig,
                           GenericSignature *derivedSig,
                           Optional<SubstitutionMap> derivedSubs,
                           LazyResolver *resolver);

  /// Combine two substitution maps as follows.
  ///
  /// The result is written in terms of the generic parameters of 'baseSig'.
  ///
  /// Generic parameters with a depth less than 'baseDepth' come from
  /// 'baseSubs'.
  ///
  /// Generic parameters with a depth greater than 'baseDepth' come from
  /// 'origSubs', but are looked up starting with a depth of 'origDepth'.
  static SubstitutionMap
  combineSubstitutionMaps(const SubstitutionMap &baseSubMap,
                          const SubstitutionMap &origSubMap,
                          unsigned baseDepth,
                          unsigned origDepth,
                          GenericSignature *baseSig);
};

} // end namespace swift

#endif
