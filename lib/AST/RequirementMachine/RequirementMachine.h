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
#include "llvm/ADT/DenseMap.h"
#include <vector>

#include "PropertyMap.h"
#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

namespace llvm {
class raw_ostream;
}

namespace swift {

class ASTContext;
class AssociatedTypeDecl;
class CanType;
class GenericTypeParamType;
class LayoutConstraint;
class ProtocolDecl;
class Requirement;
class Type;
class UnifiedStatsReporter;

namespace rewriting {
class RewriteContext;

/// Wraps a rewrite system with higher-level operations in terms of
/// generic signatures and interface types.
class RequirementMachine final {
  friend class swift::ASTContext;

  CanGenericSignature Sig;

  RewriteContext &Context;
  RewriteSystem System;
  PropertyMap Map;

  bool Debug = false;
  bool Complete = false;
  unsigned RequirementMachineStepLimit;
  unsigned RequirementMachineDepthLimit;

  UnifiedStatsReporter *Stats;

  /// All conformance access paths computed so far.
  llvm::DenseMap<std::pair<CanType, ProtocolDecl *>,
                 ConformanceAccessPath> ConformanceAccessPaths;

  /// Conformance access paths computed during the last round. All elements
  /// have the same length. If a conformance access path of greater length
  /// is requested, we refill CurrentConformanceAccessPaths with all paths of
  /// length N+1, and add them to the ConformanceAccessPaths map.
  std::vector<std::pair<CanType, ConformanceAccessPath>>
      CurrentConformanceAccessPaths;

  explicit RequirementMachine(RewriteContext &rewriteCtx);

  RequirementMachine(const RequirementMachine &) = delete;
  RequirementMachine(RequirementMachine &&) = delete;
  RequirementMachine &operator=(const RequirementMachine &) = delete;
  RequirementMachine &operator=(RequirementMachine &&) = delete;

  void addGenericSignature(CanGenericSignature sig);

  bool isComplete() const;
  void computeCompletion();

  MutableTerm getLongestValidPrefix(const MutableTerm &term) const;

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
  Type getSuperclassBound(Type depType) const;
  bool isConcreteType(Type depType) const;
  Type getConcreteType(Type depType) const;
  bool areSameTypeParameterInContext(Type depType1, Type depType2) const;
  bool isCanonicalTypeInContext(Type type) const;
  Type getCanonicalTypeInContext(Type type,
                      TypeArrayView<GenericTypeParamType> genericParams) const;
  ConformanceAccessPath getConformanceAccessPath(Type type,
                                                 ProtocolDecl *protocol);
  TypeDecl *lookupNestedType(Type depType, Identifier name) const;

  void verify(const MutableTerm &term) const;
  void dump(llvm::raw_ostream &out) const;
};

} // end namespace rewriting

} // end namespace swift

#endif