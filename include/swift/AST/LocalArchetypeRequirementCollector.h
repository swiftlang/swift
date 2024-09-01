//===--- LocalArchetypeRequirementCollector.h -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file has utility code for extending a generic signature with opened
// existentials and shape classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LOCAL_ARCHETYPE_REQUIREMENT_COLLECTOR_H
#define SWIFT_AST_LOCAL_ARCHETYPE_REQUIREMENT_COLLECTOR_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Types.h"

namespace swift {

struct LocalArchetypeRequirementCollector {
  const ASTContext &Context;
  GenericSignature OuterSig;
  unsigned Depth;

  /// The lists of new parameters and requirements to add to the signature.
  SmallVector<GenericTypeParamType *, 2> Params;
  SmallVector<Requirement, 2> Requirements;

  LocalArchetypeRequirementCollector(const ASTContext &ctx, GenericSignature sig);

  void addOpenedExistential(Type constraint);
  void addOpenedElement(CanGenericTypeParamType shapeClass);

  GenericTypeParamType *addParameter();
};

struct MapLocalArchetypesOutOfContext {
  GenericSignature baseGenericSig;
  ArrayRef<GenericEnvironment *> capturedEnvs;

  MapLocalArchetypesOutOfContext(GenericSignature baseGenericSig,
                                 ArrayRef<GenericEnvironment *> capturedEnvs)
      : baseGenericSig(baseGenericSig), capturedEnvs(capturedEnvs) {}

  Type getInterfaceType(Type interfaceTy, GenericEnvironment *genericEnv) const;
  Type operator()(SubstitutableType *type) const;
};

Type mapLocalArchetypesOutOfContext(Type type,
                                    GenericSignature baseGenericSig,
                                    ArrayRef<GenericEnvironment *> capturedEnvs);

struct MapIntoLocalArchetypeContext {
  GenericEnvironment *baseGenericEnv;
  ArrayRef<GenericEnvironment *> capturedEnvs;

  MapIntoLocalArchetypeContext(GenericEnvironment *baseGenericEnv,
                               ArrayRef<GenericEnvironment *> capturedEnvs)
      : baseGenericEnv(baseGenericEnv), capturedEnvs(capturedEnvs) {}

  Type operator()(SubstitutableType *type) const;
};

GenericSignature buildGenericSignatureWithCapturedEnvironments(
    ASTContext &ctx,
    GenericSignature sig,
    ArrayRef<GenericEnvironment *> capturedEnvs);

SubstitutionMap buildSubstitutionMapWithCapturedEnvironments(
    SubstitutionMap baseSubMap,
    GenericSignature genericSigWithCaptures,
    ArrayRef<GenericEnvironment *> capturedEnvs);

}

#endif // SWIFT_AST_LOCAL_ARCHETYPE_REQUIREMENT_COLLECTOR_H