//===--- AccessRequests.h - Access{Level,Scope} Requests -----*- C++ -*-===//
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
//  This file defines access-control requests.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_ACCESS_REQUESTS_H
#define SWIFT_ACCESS_REQUESTS_H

#include "swift/AST/AccessScope.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SimpleRequest.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/Hashing.h"

namespace swift {

class AbstractStorageDecl;
class ExtensionDecl;
class ValueDecl;

/// Request the AccessLevel of the given ValueDecl.
class AccessLevelRequest :
    public SimpleRequest<AccessLevelRequest,
                         CacheKind::SeparatelyCached,
                         AccessLevel,
                         ValueDecl *> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend class SimpleRequest;

  // Evaluation.
  llvm::Expected<AccessLevel> evaluate(Evaluator &evaluator,
                                       ValueDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<AccessLevel> getCachedResult() const;
  void cacheResult(AccessLevel value) const;
};

/// Request the setter AccessLevel of the given AbstractStorageDecl,
/// which may be lower than its normal AccessLevel, and determines
/// the accessibility of mutating accessors.
class SetterAccessLevelRequest :
    public SimpleRequest<SetterAccessLevelRequest,
                         CacheKind::SeparatelyCached,
                         AccessLevel,
                         AbstractStorageDecl *> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend class SimpleRequest;

  // Evaluation.
  llvm::Expected<AccessLevel>
  evaluate(Evaluator &evaluator, AbstractStorageDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<AccessLevel> getCachedResult() const;
  void cacheResult(AccessLevel value) const;
};

/// Request the Default and Max AccessLevels of the given ExtensionDecl.
class DefaultAndMaxAccessLevelRequest :
    public SimpleRequest<DefaultAndMaxAccessLevelRequest,
                         CacheKind::SeparatelyCached,
                         std::pair<AccessLevel, AccessLevel>,
                         ExtensionDecl *> {
public:
  using SimpleRequest::SimpleRequest;
  using DefaultAndMax = std::pair<AccessLevel, AccessLevel>;
private:
  friend class SimpleRequest;

  // Evaluation.
  llvm::Expected<DefaultAndMax>
  evaluate(Evaluator &evaluator, ExtensionDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<DefaultAndMax> getCachedResult() const;
  void cacheResult(DefaultAndMax value) const;
};

/// The zone number for access-control requests.
#define SWIFT_ACCESS_REQUESTS_TYPEID_ZONE 11

#define SWIFT_TYPEID_ZONE SWIFT_ACCESS_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/AccessTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Set up reporting of evaluated requests.
#define SWIFT_TYPEID(RequestType)                                \
template<>                                                       \
inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,  \
                            const RequestType &request) {        \
  ++stats.getFrontendCounters().RequestType;                     \
}
#include "swift/AST/AccessTypeIDZone.def"
#undef SWIFT_TYPEID

} // end namespace swift

#endif // SWIFT_ACCESS_REQUESTS_H
