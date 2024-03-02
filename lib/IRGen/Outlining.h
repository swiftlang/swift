//===--- Outlining.h - Value operation outlining ----------------*- C++ -*-===//
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
// This file defines interfaces for outlined value witnesses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_OUTLINING_H
#define SWIFT_IRGEN_OUTLINING_H

#include "IRGen.h"
#include "LocalTypeDataKind.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/GenericRequirement.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/MapVector.h"

namespace llvm {
  class Value;
  class Type;
}

namespace swift {
class CanGenericSignature;
class CanType;
enum IsInitialization_t : bool;
enum IsTake_t : bool;
class SILType;
class NominalTypeDecl;

namespace irgen {
class Address;
class Explosion;
class IRGenFunction;
class IRGenModule;
class TypeInfo;

enum LayoutIsNeeded_t : bool {
  LayoutIsNotNeeded = false,
  LayoutIsNeeded = true
};

enum DeinitIsNeeded_t : bool {
  DeinitIsNotNeeded = false,
  DeinitIsNeeded = true
};

/// A helper class for emitting outlined value operations.
///
/// The use-pattern for this class is:
///   - construct it
///   - collect all the metadata that will be required in order to perform
///     the value operations
///   - emit the call to the outlined copy/destroy helper
class OutliningMetadataCollector {
public:
  SILType T;
  IRGenFunction &IGF;
  const unsigned needsLayout : 1;
  const unsigned needsDeinit : 1;

private:
  llvm::MapVector<LocalTypeDataKey, llvm::Value *> Values;
  llvm::MapVector<GenericRequirement, llvm::Value *> Requirements;
  std::optional<SubstitutionMap> Subs;
  friend class IRGenModule;

public:
  OutliningMetadataCollector(SILType T, IRGenFunction &IGF,
                             LayoutIsNeeded_t needsLayout,
                             DeinitIsNeeded_t needsDeinitTypes);

  unsigned size() const {
    return Subs.has_value() ? Requirements.size() : Values.size();
  }

  // If any local type data is needed for \p type, add it.
  //
  // NOTE: To be called from TypeData instances.
  void collectTypeMetadata(SILType type);

  void emitCallToOutlinedCopy(Address dest, Address src,
                              SILType T, const TypeInfo &ti,
                              IsInitialization_t isInit, IsTake_t isTake) const;
  void emitCallToOutlinedDestroy(Address addr, SILType T,
                                 const TypeInfo &ti) const;
  void emitCallToOutlinedRelease(Address addr, SILType T, const TypeInfo &ti,
                                 Atomicity atomicity) const;

  void addPolymorphicArguments(SmallVectorImpl<llvm::Value *> &args) const;
  void
  addPolymorphicParameterTypes(SmallVectorImpl<llvm::Type *> &paramTys) const;
  void bindPolymorphicParameters(IRGenFunction &helperIGF,
                                 Explosion &params) const;

private:
  void collectTypeMetadataForLayout(SILType type);
  void collectTypeMetadataForDeinit(SILType type);
  void collectFormalTypeMetadata(CanType type);
  void collectRepresentationTypeMetadata(SILType ty);
};

std::pair<CanType, CanGenericSignature>
getTypeAndGenericSignatureForManglingOutlineFunction(SILType type);


}
}

#endif
