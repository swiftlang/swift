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
// SubstitutionMaps are constructed by calling the getSubstitutionMap() method
// on a GenericSignature or GenericEnvironment.
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

class GenericSignature;
class GenericEnvironment;
class SubstitutableType;

template<class Type> class CanTypeWrapper;
typedef CanTypeWrapper<SubstitutableType> CanSubstitutableType;

enum class CombineSubstitutionMaps {
  AtDepth,
  AtIndex
};

class SubstitutionMap {
  /// The generic signature for which we are performing substitutions.
  GenericSignature *genericSig;

  // FIXME: Switch to a more efficient representation.
  llvm::DenseMap<SubstitutableType *, Type> subMap;
  llvm::DenseMap<TypeBase *, SmallVector<ProtocolConformanceRef, 1>>
    conformanceMap;

public:
  SubstitutionMap()
    : SubstitutionMap(static_cast<GenericSignature *>(nullptr)) { }

  SubstitutionMap(GenericSignature *genericSig)
    : genericSig(genericSig) { }

  SubstitutionMap(GenericEnvironment *genericEnv);

  /// Retrieve the generic signature describing the environment in which
  /// substitutions occur.
  GenericSignature *getGenericSignature() const { return genericSig; }

  Optional<ProtocolConformanceRef>
  lookupConformance(CanType type, ProtocolDecl *proto) const;

  bool empty() const {
    return subMap.empty();
  }

  /// Query whether any replacement types in the map contain archetypes.
  bool hasArchetypes() const;

  /// Query whether any replacement types in the map contain an opened
  /// existential.
  bool hasOpenedExistential() const;

  /// Query whether any replacement type sin the map contain dynamic Self.
  bool hasDynamicSelf() const;

  /// Apply a substitution to all replacement types in the map. Does not
  /// change keys.
  SubstitutionMap subst(const SubstitutionMap &subMap) const;

  /// Apply a substitution to all replacement types in the map. Does not
  /// change keys.
  SubstitutionMap subst(TypeSubstitutionFn subs,
                        LookupConformanceFn conformances) const;

  /// Create a substitution map for a protocol conformance.
  static SubstitutionMap
  getProtocolSubstitutions(ProtocolDecl *protocol,
                           Type selfType,
                           ProtocolConformanceRef conformance);

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
  /// The result is written in terms of the generic parameters of 'genericSig'.
  ///
  /// Generic parameters with a depth or index less than 'firstDepthOrIndex'
  /// come from 'firstSubMap'.
  ///
  /// Generic parameters with a depth greater than 'firstDepthOrIndex' come
  /// from 'secondSubMap', but are looked up starting with a depth or index of
  /// 'secondDepthOrIndex'.
  ///
  /// The 'how' parameter determines if we're looking at the depth or index.
  static SubstitutionMap
  combineSubstitutionMaps(const SubstitutionMap &firstSubMap,
                          const SubstitutionMap &secondSubMap,
                          CombineSubstitutionMaps how,
                          unsigned baseDepthOrIndex,
                          unsigned origDepthOrIndex,
                          GenericSignature *genericSig);

  /// Verify that this substitution map is valid.
  void verify() const;

  /// Dump the contents of this substitution map for debugging purposes.
  void dump(llvm::raw_ostream &out) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in the debugger");

private:
  friend class GenericSignature;
  friend class GenericEnvironment;
  friend struct QuerySubstitutionMap;

  /// Look up the replacement for the given type parameter or interface type.
  /// Note that this only finds replacements for maps that are directly
  /// stored inside the map. In most cases, you should call Type::subst()
  /// instead, since that will resolve member types also.
  Type lookupSubstitution(CanSubstitutableType type) const;

  // You should not need to call these directly to build SubstitutionMaps;
  // instead, use GenericSignature::getSubstitutionMap() or
  // GenericEnvironment::getSubstitutionMap().

  void addSubstitution(CanSubstitutableType type, Type replacement);
  void addConformance(CanType type, ProtocolConformanceRef conformance);
};

} // end namespace swift

#endif
