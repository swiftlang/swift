//===--- SubstitutionMapStorage.h - Substitution Map Storage ----*- C++ -*-===//
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
// This file defines the SubstitutionMap::Storage class, which is used as the
// backing storage for SubstitutionMap.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_SUBSTITUTION_MAP_STORAGE_H
#define SWIFT_AST_SUBSTITUTION_MAP_STORAGE_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

class SubstitutionMap::Storage final
  : public llvm::FoldingSetNode,
    llvm::TrailingObjects<Storage, Type, ProtocolConformanceRef>
{
  friend TrailingObjects;

  /// The generic signature for which we are performing substitutions.
  GenericSignature genericSig;

  /// The number of conformance requirements, cached to avoid constantly
  /// recomputing it on conformance-buffer access.
  const unsigned numConformanceRequirements;

  Storage() = delete;

  Storage(GenericSignature genericSig,
          ArrayRef<Type> replacementTypes,
          ArrayRef<ProtocolConformanceRef> conformances);

  friend class SubstitutionMap;

private:
  unsigned getNumReplacementTypes() const {
    return genericSig.getGenericParams().size();
  }

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return getNumReplacementTypes();
  }

  size_t numTrailingObjects(OverloadToken<ProtocolConformanceRef>) const {
    return numConformanceRequirements;
  }

public:
  /// Form storage for the given generic signature and its replacement
  /// types and conformances.
  static Storage *get(GenericSignature genericSig,
                      ArrayRef<Type> replacementTypes,
                      ArrayRef<ProtocolConformanceRef> conformances);

  /// Retrieve the generic signature that describes the shape of this
  /// storage.
  GenericSignature getGenericSignature() const { return genericSig; }

  /// Retrieve the array of replacement types, which line up with the
  /// generic parameters.
  ///
  /// Note that the types may be null, for cases where the generic parameter
  /// is concrete but hasn't been queried yet.
  ArrayRef<Type> getReplacementTypes() const {
    return llvm::ArrayRef(getTrailingObjects<Type>(), getNumReplacementTypes());
  }

  MutableArrayRef<Type> getReplacementTypes() {
    return MutableArrayRef<Type>(getTrailingObjects<Type>(),
                                 getNumReplacementTypes());
  }

  /// Retrieve the array of protocol conformances, which line up with the
  /// requirements of the generic signature.
  ArrayRef<ProtocolConformanceRef> getConformances() const {
    return llvm::ArrayRef(getTrailingObjects<ProtocolConformanceRef>(),
                          numConformanceRequirements);
  }
  MutableArrayRef<ProtocolConformanceRef> getConformances() {
    return MutableArrayRef<ProtocolConformanceRef>(
                              getTrailingObjects<ProtocolConformanceRef>(),
                              numConformanceRequirements);
  }

  /// Profile the substitution map storage, for use with LLVM's FoldingSet.
  void Profile(llvm::FoldingSetNodeID &id) const {
    Profile(id, getGenericSignature(), getReplacementTypes(),
            getConformances());
  }

  /// Profile the substitution map storage, for use with LLVM's FoldingSet.
  static void Profile(llvm::FoldingSetNodeID &id,
                      GenericSignature genericSig,
                      ArrayRef<Type> replacementTypes,
                      ArrayRef<ProtocolConformanceRef> conformances);
};

}

#endif // SWIFT_AST_SUBSTITUTION_MAP_STORAGE_H
