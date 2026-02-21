//===--- GenArchetype.h - Swift IR generation for archetypes ----*- C++ -*-===//
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
//  This file provides the private interface to the archetype emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENARCHETYPE_H
#define SWIFT_IRGEN_GENARCHETYPE_H

#include "swift/AST/Types.h"
#include "llvm/ADT/STLExtras.h"

namespace llvm {
  class Value;
}

namespace swift {
  class AssociatedTypeDecl;
  class ProtocolDecl;
  class SILType;

namespace irgen {
  class Address;
  class IRGenFunction;
  class DynamicMetadataRequest;
  class MetadataResponse;

  using GetTypeParameterInContextFn =
    llvm::function_ref<CanType(CanType type)>;

  /// Emit a type metadata reference for an archetype.
  MetadataResponse emitArchetypeTypeMetadataRef(IRGenFunction &IGF,
                                                CanArchetypeType archetype,
                                                DynamicMetadataRequest request);

  /// Emit a witness table reference.
  llvm::Value *emitArchetypeWitnessTableRef(IRGenFunction &IGF,
                                            CanArchetypeType archetype,
                                            ProtocolDecl *protocol);

  /// Emit a metadata reference for an associated type of an archetype.
  MetadataResponse emitAssociatedTypeMetadataRef(IRGenFunction &IGF,
                                                 CanArchetypeType origin,
                                                 AssociatedTypeDecl *assocType,
                                                 DynamicMetadataRequest request);

  /// Emit a dynamic metatype lookup for the given archetype.
  llvm::Value *emitDynamicTypeOfOpaqueArchetype(IRGenFunction &IGF,
                                                Address archetypeAddr,
                                                SILType archetypeType);
  
  /// Emit a lookup for an opaque result type's metadata.
  MetadataResponse emitOpaqueTypeMetadataRef(IRGenFunction &IGF,
                                             CanOpaqueTypeArchetypeType archetype,
                                             DynamicMetadataRequest request);
  /// Emit a lookup for an opaque result type's protocol conformance.
  llvm::Value *emitOpaqueTypeWitnessTableRef(IRGenFunction &IGF,
                                             CanOpaqueTypeArchetypeType archetype,
                                             ProtocolDecl *protocol);
} // end namespace irgen
} // end namespace swift

#endif
