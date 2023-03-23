//===--- InFlightSubstitution.h - In-flight substitution data ---*- C++ -*-===//
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
// This file defines the InFlightSubstitution structure, which captures
// all the information about a type substitution that's currently in
// progress.  For now, this is meant to be an internal implementation
// detail of the substitution system, and other systems should not use
// it (unless they are part of the extended substitution system, such as
// the SIL type substituter)
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_INFLIGHTSUBSTITUTION_H
#define SWIFT_AST_INFLIGHTSUBSTITUTION_H

#include "swift/AST/SubstitutionMap.h"

namespace swift {
class SubstitutionMap;

class InFlightSubstitution {
  SubstOptions Options;
  TypeSubstitutionFn BaselineSubstType;
  LookupConformanceFn BaselineLookupConformance;

public:
  InFlightSubstitution(TypeSubstitutionFn substType,
                       LookupConformanceFn lookupConformance,
                       SubstOptions options)
    : Options(options),
      BaselineSubstType(substType),
      BaselineLookupConformance(lookupConformance) {}

  InFlightSubstitution(const InFlightSubstitution &) = delete;
  InFlightSubstitution &operator=(const InFlightSubstitution &) = delete;

  Type substType(SubstitutableType *ty) {
    return BaselineSubstType(ty);
  }

  ProtocolConformanceRef lookupConformance(CanType dependentType,
                                           Type conformingReplacementType,
                                           ProtocolDecl *conformedProtocol) {
    return BaselineLookupConformance(dependentType,
                                     conformingReplacementType,
                                     conformedProtocol);
  }

  class OptionsAdjustmentScope {
    InFlightSubstitution &IFS;
    SubstOptions SavedOptions;

  public:
    OptionsAdjustmentScope(InFlightSubstitution &IFS, SubstOptions newOptions)
      : IFS(IFS), SavedOptions(IFS.Options) {
      IFS.Options = newOptions;
    }

    OptionsAdjustmentScope(const OptionsAdjustmentScope &) = delete;
    OptionsAdjustmentScope &operator=(const OptionsAdjustmentScope &) = delete;

    ~OptionsAdjustmentScope() {
      IFS.Options = SavedOptions;
    }
  };

  template <class Fn>
  auto withNewOptions(SubstOptions options, Fn &&fn)
      -> decltype(std::forward<Fn>(fn)()) {
    OptionsAdjustmentScope scope(*this, options);
    return std::forward<Fn>(fn)();
  }

  SubstOptions getOptions() const {
    return Options;
  }

  bool shouldSubstituteOpaqueArchetypes() const {
    return Options.contains(SubstFlags::SubstituteOpaqueArchetypes);
  }

  /// Is the given type invariant to substitution?
  bool isInvariant(Type type) const;
};

/// A helper classes that provides stable storage for the query
/// functions against a SubstitutionMap.
struct InFlightSubstitutionViaSubMapHelper {
  QuerySubstitutionMap QueryType;
  LookUpConformanceInSubstitutionMap QueryConformance;

  InFlightSubstitutionViaSubMapHelper(SubstitutionMap subMap)
    : QueryType{subMap}, QueryConformance(subMap) {}
};
class InFlightSubstitutionViaSubMap :
  private InFlightSubstitutionViaSubMapHelper,
  public InFlightSubstitution {

public:
  InFlightSubstitutionViaSubMap(SubstitutionMap subMap,
                                SubstOptions options)
    : InFlightSubstitutionViaSubMapHelper(subMap),
      InFlightSubstitution(QueryType, QueryConformance, options) {}
};

} // end namespace swift

#endif
