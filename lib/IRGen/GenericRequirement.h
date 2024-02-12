//===--- GenericRequirement.h - Generic requirements ------------*- C++ -*-===//
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
// This class describes types for working with requirements of generic
// signatures and the layout of the generic arguments section of
// generic type metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENERICREQUIREMENT_H
#define SWIFT_IRGEN_GENERICREQUIREMENT_H

#include "swift/AST/Type.h"
#include "swift/IRGen/GenericRequirement.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
class Value;
}

namespace swift {
class CanGenericSignature;
enum class MetadataState : size_t;
class ModuleDecl;
class NominalTypeDecl;
class ProtocolDecl;
class SubstitutionMap;

namespace irgen {
class Address;
class IRGenFunction;
class IRGenModule;
class DynamicMetadataRequest;

using RequirementCallback =
  llvm::function_ref<void(GenericRequirement requirement)>;

/// Enumerate the generic requirements imposed by a generic signature.
void enumerateGenericSignatureRequirements(CanGenericSignature signature,
                                           const RequirementCallback &callback);

/// Given a substitution map and a generic requirement for the
/// substitution map's input generic signature, emit the required value.
llvm::Value *
emitGenericRequirementFromSubstitutions(IRGenFunction &IGF,
                                        GenericRequirement requirement,
                                        MetadataState metadataState,
                                        SubstitutionMap subs,
                                        bool onHeapPacks=false);

void emitInitOfGenericRequirementsBuffer(IRGenFunction &IGF,
                                         ArrayRef<GenericRequirement> reqts,
                                         Address buffer,
                                         MetadataState metadataState,
                                         SubstitutionMap subs,
                                         bool onHeapPacks=false);

/// Given a required value, map the requirement into the given
/// context and bind the value.
void bindGenericRequirement(IRGenFunction &IGF,
                            GenericRequirement requirement,
                            llvm::Value *requiredValue,
                            MetadataState metadataState,
                            SubstitutionMap subs);

void bindFromGenericRequirementsBuffer(IRGenFunction &IGF,
                                       ArrayRef<GenericRequirement> reqts,
                                       Address buffer,
                                       MetadataState metadataState,
                                       SubstitutionMap subs);

void bindPolymorphicArgumentsFromComponentIndices(IRGenFunction &IGF,
                                                  GenericEnvironment *genericEnv,
                                                  ArrayRef<GenericRequirement> requirements,
                                                  llvm::Value *args,
                                                  llvm::Value *size,
                                                  bool hasSubscriptIndices);


/// A class describing the layout of the generic requirements of a
/// nominal type metadata.
///
/// The generic requirements are always laid out as a sequence of shape
/// parameters, followed by type metadata and witness tables.
class GenericTypeRequirements {
  llvm::SmallVector<GenericRequirement, 4> Requirements;

public:
  GenericTypeRequirements(IRGenModule &IGM, NominalTypeDecl *decl);
  GenericTypeRequirements(IRGenModule &IGM, GenericSignature sig);

  /// Return the layout chunks.
  ArrayRef<GenericRequirement> getRequirements() const {
    return Requirements;
  }

  bool empty() const { return Requirements.empty(); }

  void emitInitOfBuffer(IRGenFunction &IGF, SubstitutionMap subs,
                        Address buffer);

  void bindFromBuffer(IRGenFunction &IGF, Address buffer, MetadataState state,
                      SubstitutionMap subs);
};

} // end namespace irgen
} // end namespace swift

#endif
