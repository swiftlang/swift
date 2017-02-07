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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SubstitutableType;

template<class Type> class CanTypeWrapper;
typedef CanTypeWrapper<SubstitutableType> CanSubstitutableType;

class SubstitutionMap {
  using ParentType = std::pair<CanType, AssociatedTypeDecl *>;

  llvm::DenseMap<SubstitutableType *, Type> subMap;
  llvm::DenseMap<TypeBase *, SmallVector<ProtocolConformanceRef, 1>>
    conformanceMap;
  llvm::DenseMap<TypeBase *, SmallVector<ParentType, 1>> parentMap;

  // Call the given function for each parent of the given type. The
  // function \c fn should return an \c Optional<T>. \c forEachParent() will
  // return the first non-empty \C Optional<T> returned by \c fn.
  template<typename T>
  Optional<T> forEachParent(
                CanType type,
                llvm::SmallPtrSetImpl<CanType> &visitedParents,
                llvm::function_ref<Optional<T>(CanType,
                                               AssociatedTypeDecl *)> fn) const;

  // Call the given function for each conformance of the given type. The
  // function \c fn should return an \c Optional<T>. \c forEachConformance()
  // will return the first non-empty \C Optional<T> returned by \c fn.
  template<typename T>
  Optional<T> forEachConformance(
                  CanType type,
                  llvm::SmallPtrSetImpl<CanType> &visitedParents,
                  llvm::function_ref<Optional<T>(ProtocolConformanceRef)> fn)
                const;

public:
  Optional<ProtocolConformanceRef>
  lookupConformance(
                CanType type, ProtocolDecl *proto,
                llvm::SmallPtrSetImpl<CanType> *visitedParents = nullptr) const;

  /// Retrieve the conformances for the given type.
  ArrayRef<ProtocolConformanceRef> getConformances(CanType type) const;

  void addSubstitution(CanSubstitutableType type, Type replacement);

  Type lookupSubstitution(CanSubstitutableType type) const;

  void addConformance(CanType type, ProtocolConformanceRef conformance);

  void addParent(CanType type, CanType parent,
                 AssociatedTypeDecl *assocType);
  
  bool empty() const {
    return subMap.empty();
  }

  /// Query whether any replacement types in the map contain archetypes.
  bool hasArchetypes() const;

  /// Query whether any replacement type sin the map contain dynamic Self.
  bool hasDynamicSelf() const;

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

  /// Dump the contents of this substitution map for debugging purposes.
  void dump(llvm::raw_ostream &out) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in the debugger");
};

} // end namespace swift

#endif
