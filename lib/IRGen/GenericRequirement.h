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
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/DenseMapInfo.h"

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

/// An abstract generic requirement.
struct GenericRequirement {
  CanType TypeParameter;
  ProtocolDecl *Protocol;
};

using RequirementCallback =
  llvm::function_ref<void(GenericRequirement requirement)>;

/// Enumerate the generic requirements imposed by a generic signature.
void enumerateGenericSignatureRequirements(CanGenericSignature signature,
                                           const RequirementCallback &callback);

/// Given an array of substitutions that parallel the dependent
/// signature for which a requirement was emitted, emit the required
/// value.
llvm::Value *
emitGenericRequirementFromSubstitutions(IRGenFunction &IGF,
                                        CanGenericSignature signature,
                                        ModuleDecl &module,
                                        GenericRequirement requirement,
                                        SubstitutionMap subs);

using EmitGenericRequirementFn =
  llvm::function_ref<llvm::Value*(GenericRequirement reqt)>;
void emitInitOfGenericRequirementsBuffer(IRGenFunction &IGF,
                                         ArrayRef<GenericRequirement> reqts,
                                         Address buffer,
                                      EmitGenericRequirementFn emitRequirement);

using GetTypeParameterInContextFn =
  llvm::function_ref<CanType(CanType type)>;

/// Given a required value, map the requirement into the given
/// context and bind the value.
void bindGenericRequirement(IRGenFunction &IGF,
                            GenericRequirement requirement,
                            llvm::Value *requiredValue,
                            MetadataState metadataState,
                            GetTypeParameterInContextFn getInContext);

void bindFromGenericRequirementsBuffer(IRGenFunction &IGF,
                                       ArrayRef<GenericRequirement> reqts,
                                       Address buffer,
                                       MetadataState metadataState,
                                       GetTypeParameterInContextFn getInContext);


/// A class describing the layout of the generic requirements of a
/// nominal type metadata.
///
/// The generic requirements are always laid out as a sequence of type
/// metadata (corresponding to the type parameters of the context established
/// by the type, minus anything fulfillable from its parent type metadata)
/// followed by a sequence of protocol witness tables (corresponding to the
/// root conformances of the context established by the type, again minus
/// anything fulfillable from its parent type metadata).
class GenericTypeRequirements {
  NominalTypeDecl *TheDecl;
  llvm::SmallVector<GenericRequirement, 4> Requirements;

public:
  GenericTypeRequirements(IRGenModule &IGM, NominalTypeDecl *decl);

  /// Return the layout chunks.
  ArrayRef<GenericRequirement> getRequirements() const {
    return Requirements;
  }

  /// Return the number of entries required in order to store this data.
  unsigned getStorageSizeInWords() const {
    return Requirements.size();
  }

  /// Return the number of type metadata requirements.
  unsigned getNumTypeRequirements() const {
    unsigned count = 0;
    for (auto i = Requirements.begin(), e = Requirements.end(); i != e; ++i) {
      if (!i->Protocol) {
        count++;
      } else {
#ifndef NDEBUG
        // Assert that the rest of the requirements are conformance
        // requirements.
        for (++i; i != e; ++i) {
          assert(i->Protocol && "type requirement followed conformance!");
        }
#endif
        break;
      }
    }
    return count;
  }

  bool empty() const { return Requirements.empty(); }

  using FulfillmentCallback =
    llvm::function_ref<void(unsigned requirementIndex,
                            CanType type,
                            Optional<ProtocolConformanceRef> conf)>;
  void enumerateFulfillments(IRGenModule &IGM, SubstitutionMap subs,
                             FulfillmentCallback callback);

  void emitInitOfBuffer(IRGenFunction &IGF, SubstitutionMap subs,
                        Address buffer);

  void bindFromBuffer(IRGenFunction &IGF, Address buffer, MetadataState state,
                      GetTypeParameterInContextFn getInContext);
};

} // end namespace irgen
} // end namespace swift

namespace llvm {
  template <> struct DenseMapInfo<swift::irgen::GenericRequirement> {
    using GenericRequirement = swift::irgen::GenericRequirement;
    using CanTypeInfo = llvm::DenseMapInfo<swift::CanType>;
    static GenericRequirement getEmptyKey() {
      return { CanTypeInfo::getEmptyKey(), nullptr };
    }
    static GenericRequirement getTombstoneKey() {
      return { CanTypeInfo::getTombstoneKey(), nullptr };
    }
    static llvm::hash_code getHashValue(GenericRequirement req) {
      return hash_combine(CanTypeInfo::getHashValue(req.TypeParameter),
                          hash_value(req.Protocol));
    }
    static bool isEqual(GenericRequirement lhs, GenericRequirement rhs) {
      return (lhs.TypeParameter == rhs.TypeParameter &&
              lhs.Protocol == rhs.Protocol);
    }
  };
}

#endif
