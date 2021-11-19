//===--- RequirementLowering.h - Building rules from requirements ---------===//
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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>
#include "RewriteContext.h"
#include "Symbol.h"
#include "Term.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class AssociatedTypeDecl;
class ProtocolDecl;
class Requirement;

namespace rewriting {

// Entry points used by AbstractGenericSignatureRequest and
// InferredGenericSignatureRequest; see RequiremetnLowering.cpp for
// documentation
// comments.

void desugarRequirement(Requirement req, SmallVectorImpl<Requirement> &result);

void inferRequirements(Type type, SourceLoc loc, ModuleDecl *module,
                       SmallVectorImpl<StructuralRequirement> &result);

void realizeRequirement(Requirement req, RequirementRepr *reqRepr,
                        ModuleDecl *moduleForInference,
                        SmallVectorImpl<StructuralRequirement> &result);

void realizeInheritedRequirements(TypeDecl *decl, Type type,
                                  ModuleDecl *moduleForInference,
                                  SmallVectorImpl<StructuralRequirement> &result);

/// A utility class for bulding rewrite rules from the top-level requirements
/// of a generic signature.
///
/// This also collects requirements from the transitive closure of all protocols
/// appearing on the right hand side of conformance requirements.
struct RuleBuilder {
  RewriteContext &Context;
  bool Dump;

  /// The keys are the unique protocols we've added so far. The value indicates
  /// whether the protocol's SCC is an initial component for the rewrite system.
  ///
  /// A rewrite system built from a generic signature does not have any initial
  /// protocols.
  ///
  /// A rewrite system built from a protocol SCC has the protocols of the SCC
  /// itself as initial protocols.
  ///
  /// If a protocol is an initial protocol, we use its structural requirements
  /// instead of its requirement signature as the basis of its rewrite rules.
  ///
  /// This is what breaks the cycle in requirement signature computation for a
  /// group of interdependent protocols.
  llvm::DenseMap<const ProtocolDecl *, bool> ProtocolMap;
  std::vector<const ProtocolDecl *> Protocols;

  /// New rules to add which will be marked 'permanent'. These are rules for
  /// introducing associated types, and relationships between layout,
  /// superclass and concrete type symbols. They are not eliminated by
  /// homotopy reduction, since they are always added when the rewrite system
  /// is built.
  std::vector<std::pair<MutableTerm, MutableTerm>> PermanentRules;

  /// New rules derived from requirements written by the user, which can be
  /// eliminated by homotopy reduction.
  std::vector<std::pair<MutableTerm, MutableTerm>> RequirementRules;

  CanType getConcreteSubstitutionSchema(CanType concreteType,
                                        const ProtocolDecl *proto,
                                        SmallVectorImpl<Term> &result);

  RuleBuilder(RewriteContext &ctx, bool dump) : Context(ctx), Dump(dump) {}
  void addRequirements(ArrayRef<Requirement> requirements);
  void addRequirements(ArrayRef<StructuralRequirement> requirements);
  void addProtocols(ArrayRef<const ProtocolDecl *> proto);
  void addProtocol(const ProtocolDecl *proto,
                   bool initialComponent);
  void addAssociatedType(const AssociatedTypeDecl *type,
                         const ProtocolDecl *proto);
  void addRequirement(const Requirement &req,
                      const ProtocolDecl *proto);
  void addRequirement(const StructuralRequirement &req,
                      const ProtocolDecl *proto);
  void collectRulesFromReferencedProtocols();
};

} // end namespace rewriting

} // end namespace swift

#endif
