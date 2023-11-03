//===--- RequirementLowering.h - Requirement inference and desugaring -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REQUIREMENTLOWERING_H
#define SWIFT_REQUIREMENTLOWERING_H

#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>
#include "Diagnostics.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class AssociatedTypeDecl;
class ProtocolDecl;
class ProtocolTypeAlias;
class Requirement;

namespace rewriting {

// Entry points used by AbstractGenericSignatureRequest and
// InferredGenericSignatureRequest; see RequirementLowering.cpp for
// documentation
// comments.

void desugarRequirement(Requirement req, SourceLoc loc,
                        SmallVectorImpl<Requirement> &result,
                        SmallVectorImpl<RequirementError> &errors);

void inferRequirements(Type type, SourceLoc loc,
                       ModuleDecl *module, DeclContext *dc,
                       SmallVectorImpl<StructuralRequirement> &result);

void realizeRequirement(DeclContext *dc,
                        Requirement req, RequirementRepr *reqRepr,
                        bool shouldInferRequirements,
                        SmallVectorImpl<StructuralRequirement> &result,
                        SmallVectorImpl<RequirementError> &errors);

void realizeInheritedRequirements(TypeDecl *decl, Type type,
                                  bool shouldInferRequirements,
                                  SmallVectorImpl<StructuralRequirement> &result,
                                  SmallVectorImpl<RequirementError> &errors);

void expandDefaultRequirements(ASTContext &ctx,
                               SmallVectorImpl<Type> const& subjects,
                               SmallVectorImpl<StructuralRequirement> &result,
                               SmallVectorImpl<RequirementError> &errors);

// Defined in ConcreteContraction.cpp.
bool performConcreteContraction(
    ArrayRef<StructuralRequirement> requirements,
    SmallVectorImpl<StructuralRequirement> &result,
    SmallVectorImpl<RequirementError> &errors,
    bool debug);

} // end namespace rewriting

} // end namespace swift

#endif
