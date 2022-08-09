//===--- RequirementMachine.h - Generics with term rewriting ----*- C++ -*-===//
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

#ifndef SWIFT_REQUIREMENTMACHINE_H
#define SWIFT_REQUIREMENTMACHINE_H

#include "swift/AST/GenericSignature.h"
#include "swift/AST/RequirementSignature.h"
#include "llvm/ADT/DenseMap.h"
#include <vector>

#include "Diagnostics.h"
#include "PropertyMap.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

namespace llvm {
class raw_ostream;
}

namespace swift {

class AbstractGenericSignatureRequest;
class ASTContext;
class AssociatedTypeDecl;
class CanType;
class GenericTypeParamType;
class InferredGenericSignatureRequest;
class LayoutConstraint;
class ProtocolDecl;
class Requirement;
class RequirementSignatureRequest;
class Type;
class UnifiedStatsReporter;

namespace rewriting {
class RewriteContext;

/// Wraps a rewrite system with higher-level operations in terms of
/// generic signatures and interface types.
class RequirementMachine final {
  friend class swift::ASTContext;
  friend class swift::rewriting::RewriteContext;
  friend class swift::RequirementSignatureRequest;
  friend class swift::AbstractGenericSignatureRequest;
  friend class swift::InferredGenericSignatureRequest;

  CanGenericSignature Sig;
  SmallVector<Type, 2> Params;

  RewriteContext &Context;
  RewriteSystem System;
  PropertyMap Map;

  bool Dump = false;
  bool Complete = false;

  /// Parameters to prevent runaway completion and property map construction.
  unsigned MaxRuleCount;
  unsigned MaxRuleLength;
  unsigned MaxConcreteNesting;

  UnifiedStatsReporter *Stats;

  /// All conformance access paths computed so far.
  llvm::DenseMap<std::pair<Term, ProtocolDecl *>,
                 ConformanceAccessPath> ConformanceAccessPaths;

  /// Conformance access paths computed during the last round. All elements
  /// have the same length. If a conformance access path of greater length
  /// is requested, we refill CurrentConformanceAccessPaths with all paths of
  /// length N+1, and add them to the ConformanceAccessPaths map.
  std::vector<std::pair<Term, ConformanceAccessPath>>
      CurrentConformanceAccessPaths;

  explicit RequirementMachine(RewriteContext &rewriteCtx);

  RequirementMachine(const RequirementMachine &) = delete;
  RequirementMachine(RequirementMachine &&) = delete;
  RequirementMachine &operator=(const RequirementMachine &) = delete;
  RequirementMachine &operator=(RequirementMachine &&) = delete;

  void checkCompletionResult(CompletionResult result) const;

  std::pair<CompletionResult, unsigned>
  initWithProtocolSignatureRequirements(
      ArrayRef<const ProtocolDecl *> protos);

  std::pair<CompletionResult, unsigned>
  initWithGenericSignature(CanGenericSignature sig);

  std::pair<CompletionResult, unsigned>
  initWithProtocolWrittenRequirements(
      ArrayRef<const ProtocolDecl *> component,
      const llvm::DenseMap<const ProtocolDecl *,
                           SmallVector<StructuralRequirement, 4>> protos);

  std::pair<CompletionResult, unsigned>
  initWithWrittenRequirements(
      ArrayRef<GenericTypeParamType *> genericParams,
      ArrayRef<StructuralRequirement> requirements);

  bool isComplete() const;

  std::pair<CompletionResult, unsigned>
  computeCompletion(RewriteSystem::ValidityPolicy policy);

  void freeze();

  void computeRequirementDiagnostics(SmallVectorImpl<RequirementError> &errors,
                                     SourceLoc signatureLoc);

  MutableTerm getLongestValidPrefix(const MutableTerm &term) const;

  void buildRequirementsFromRules(
    ArrayRef<unsigned> requirementRules,
    ArrayRef<unsigned> typeAliasRules,
    TypeArrayView<GenericTypeParamType> genericParams,
    bool reconstituteSugar,
    std::vector<Requirement> &reqs,
    std::vector<ProtocolTypeAlias> &aliases) const;

  TypeArrayView<GenericTypeParamType> getGenericParams() const {
    return TypeArrayView<GenericTypeParamType>(
      ArrayRef<Type>(Params));
  }

public:
  ~RequirementMachine();

  // Generic signature queries. Generally you shouldn't have to construct a
  // RequirementMachine instance; instead, call the corresponding methods on
  // GenericSignature, which lazily create a RequirementMachine for you.
  GenericSignature::LocalRequirements getLocalRequirements(Type depType,
                      TypeArrayView<GenericTypeParamType> genericParams) const;
  bool requiresClass(Type depType) const;
  LayoutConstraint getLayoutConstraint(Type depType) const;
  bool requiresProtocol(Type depType, const ProtocolDecl *proto) const;
  GenericSignature::RequiredProtocols getRequiredProtocols(Type depType) const;
  Type getSuperclassBound(Type depType,
                          TypeArrayView<GenericTypeParamType> genericParams) const;
  bool isConcreteType(Type depType,
                      const ProtocolDecl *proto=nullptr) const;
  Type getConcreteType(Type depType,
                       TypeArrayView<GenericTypeParamType> genericParams,
                       const ProtocolDecl *proto=nullptr) const;
  bool areReducedTypeParametersEqual(Type depType1, Type depType2) const;
  bool isReducedType(Type type) const;
  Type getReducedType(Type type,
                      TypeArrayView<GenericTypeParamType> genericParams) const;
  bool isValidTypeParameter(Type type) const;
  ConformanceAccessPath getConformanceAccessPath(Type type,
                                                 ProtocolDecl *protocol);
  TypeDecl *lookupNestedType(Type depType, Identifier name) const;

  llvm::DenseMap<const ProtocolDecl *, RequirementSignature>
  computeMinimalProtocolRequirements();

  GenericSignature
  computeMinimalGenericSignature(bool reconstituteSugar);

  ArrayRef<Rule> getLocalRules() const;

  std::string getRuleAsStringForDiagnostics(unsigned ruleID) const;

  GenericSignatureErrors getErrors() const;

  void verify(const MutableTerm &term) const;
  void dump(llvm::raw_ostream &out) const;

  DebugOptions getDebugOptions() const { return Context.getDebugOptions(); }
};

} // end namespace rewriting

} // end namespace swift

#endif
