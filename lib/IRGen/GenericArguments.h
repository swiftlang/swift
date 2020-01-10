//===--- MetadataRequest.cpp - IR generation for metadata requests --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for accessing metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENERICARGUMENTS_H
#define SWIFT_IRGEN_GENERICARGUMENTS_H

#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenArchetype.h"
#include "GenClass.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "GenericRequirement.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IRGen/Linking.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {
namespace irgen {

/// A structure for collecting generic arguments for emitting a
/// nominal metadata reference.  The structure produced here is
/// consumed by swift_getGenericMetadata() and must correspond to
/// the fill operations that the compiler emits for the bound decl.
struct GenericArguments {
  /// The values to use to initialize the arguments structure.
  SmallVector<llvm::Value *, 8> Values;
  SmallVector<llvm::Type *, 8> Types;

  static unsigned getNumGenericArguments(IRGenModule &IGM,
                                         NominalTypeDecl *nominal) {
    GenericTypeRequirements requirements(IGM, nominal);
    return requirements.getNumTypeRequirements();
  }

  void collectTypes(IRGenModule &IGM, NominalTypeDecl *nominal) {
    GenericTypeRequirements requirements(IGM, nominal);
    collectTypes(IGM, requirements);
  }

  void collectTypes(IRGenModule &IGM,
                    const GenericTypeRequirements &requirements) {
    for (auto &requirement : requirements.getRequirements()) {
      if (requirement.Protocol) {
        Types.push_back(IGM.WitnessTablePtrTy);
      } else {
        Types.push_back(IGM.TypeMetadataPtrTy);
      }
    }
  }

  void collect(IRGenFunction &IGF, CanType type) {
    auto *decl = type.getNominalOrBoundGenericNominal();
    GenericTypeRequirements requirements(IGF.IGM, decl);

    auto subs = type->getContextSubstitutionMap(IGF.IGM.getSwiftModule(), decl);
    requirements.enumerateFulfillments(
        IGF.IGM, subs,
        [&](unsigned reqtIndex, CanType type, ProtocolConformanceRef conf) {
          if (conf) {
            Values.push_back(emitWitnessTableRef(IGF, type, conf));
          } else {
            Values.push_back(IGF.emitAbstractTypeMetadataRef(type));
          }
        });

    collectTypes(IGF.IGM, decl);
    assert(Types.size() == Values.size());
  }
};

} // namespace irgen
} // namespace swift

#endif
