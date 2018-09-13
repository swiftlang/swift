//===--- TypeCheckRequests.h - Type Checking Requests -----------*- C++ -*-===//
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
//  This file defines type checking requests.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_TYPE_CHECK_REQUESTS_H
#define SWIFT_TYPE_CHECK_REQUESTS_H

#include "swift/AST/Type.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SimpleRequest.h"
#include "swift/AST/TypeResolutionStage.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

class GenericParamList;
class RequirementRepr;
class SpecializeAttr;
struct TypeLoc;

/// Display a nominal type or extension thereof.
void simple_display(
       llvm::raw_ostream &out,
       const llvm::PointerUnion<TypeDecl *, ExtensionDecl *> &value);

/// Request the type from the ith entry in the inheritance clause for the
/// given declaration.
class InheritedTypeRequest :
    public SimpleRequest<InheritedTypeRequest,
                         CacheKind::SeparatelyCached,
                         Type,
                         llvm::PointerUnion<TypeDecl *, ExtensionDecl *>,
                         unsigned,
                         TypeResolutionStage>
{
  /// Retrieve the TypeLoc for this inherited type.
  TypeLoc &getTypeLoc(llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                      unsigned index) const;

public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<Type>
  evaluate(Evaluator &evaluator,
           llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
           unsigned index,
           TypeResolutionStage stage) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Caching
  bool isCached() const;
  Optional<Type> getCachedResult() const;
  void cacheResult(Type value) const;
};

/// Request the superclass type for the given class.
class SuperclassTypeRequest :
    public SimpleRequest<SuperclassTypeRequest,
                         CacheKind::SeparatelyCached,
                         Type,
                         NominalTypeDecl *,
                         TypeResolutionStage> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<Type>
  evaluate(Evaluator &evaluator, NominalTypeDecl *classDecl,
           TypeResolutionStage stage) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const;
  Optional<Type> getCachedResult() const;
  void cacheResult(Type value) const;
};

/// Request the raw type of the given enum.
class EnumRawTypeRequest :
    public SimpleRequest<EnumRawTypeRequest,
                         CacheKind::SeparatelyCached,
                         Type,
                         EnumDecl *,
                         TypeResolutionStage> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<Type>
  evaluate(Evaluator &evaluator, EnumDecl *enumDecl,
           TypeResolutionStage stage) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const;
  Optional<Type> getCachedResult() const;
  void cacheResult(Type value) const;
};

/// Request to determine the set of declarations that were are overridden
/// by the given declaration.
class OverriddenDeclsRequest
  : public SimpleRequest<OverriddenDeclsRequest,
                         CacheKind::SeparatelyCached,
                         llvm::TinyPtrVector<ValueDecl *>,
                         ValueDecl *> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<llvm::TinyPtrVector<ValueDecl *>>
  evaluate(Evaluator &evaluator, ValueDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<llvm::TinyPtrVector<ValueDecl *>> getCachedResult() const;
  void cacheResult(llvm::TinyPtrVector<ValueDecl *> value) const;
};

/// Determine whether the given declaration is exposed to Objective-C.
class IsObjCRequest :
    public SimpleRequest<IsObjCRequest,
                         CacheKind::SeparatelyCached,
                         bool,
                         ValueDecl *> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<bool> evaluate(Evaluator &evaluator, ValueDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<bool> getCachedResult() const;
  void cacheResult(bool value) const;
};

/// Determine whether the given declaration is 'dynamic''.
class IsDynamicRequest :
    public SimpleRequest<IsDynamicRequest,
                         CacheKind::SeparatelyCached,
                         bool,
                         ValueDecl *> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<bool> evaluate(Evaluator &evaluator, ValueDecl *decl) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<bool> getCachedResult() const;
  void cacheResult(bool value) const;
};

/// Describes the owner of a where clause, from which we can extract
/// requirements.
struct WhereClauseOwner {
  /// The declaration context in which the where clause will be evaluated.
  DeclContext *dc;

  /// The source of the where clause, which can be a generic parameter list
  /// or a declaration that can have a where clause.
  llvm::PointerUnion3<GenericParamList *, Decl *, SpecializeAttr *> source;

  WhereClauseOwner(Decl *decl);

  WhereClauseOwner(DeclContext *dc, GenericParamList *genericParams)
    : dc(dc), source(genericParams) { }

  WhereClauseOwner(DeclContext *dc, SpecializeAttr *attr)
    : dc(dc), source(attr) { }

  SourceLoc getLoc() const;

  friend hash_code hash_value(const WhereClauseOwner &owner) {
    return hash_combine(hash_value(owner.dc),
                        hash_value(owner.source.getOpaqueValue()));
  }

  friend bool operator==(const WhereClauseOwner &lhs,
                         const WhereClauseOwner &rhs) {
    return lhs.dc == rhs.dc &&
           lhs.source.getOpaqueValue() == rhs.source.getOpaqueValue();
  }

  friend bool operator!=(const WhereClauseOwner &lhs,
                         const WhereClauseOwner &rhs) {
    return !(lhs == rhs);
  }
};

void simple_display(llvm::raw_ostream &out, const WhereClauseOwner &owner);

/// Retrieve a requirement from the where clause of the given declaration.
class RequirementRequest :
    public SimpleRequest<RequirementRequest,
                         CacheKind::SeparatelyCached,
                         Requirement,
                         WhereClauseOwner,
                         unsigned,
                         TypeResolutionStage> {
public:
  using SimpleRequest::SimpleRequest;

  /// Retrieve the array of requirements from the given owner.
  static MutableArrayRef<RequirementRepr> getRequirements(WhereClauseOwner);

  /// Visit each of the requirements in the given owner,
  ///
  /// \returns true after short-circuiting if the callback returned \c true
  /// for any of the requirements.
  static bool visitRequirements(
      WhereClauseOwner, TypeResolutionStage stage,
      llvm::function_ref<bool(Requirement, RequirementRepr*)> callback);

private:
  friend SimpleRequest;

  /// Retrieve the requirement this request operates on.
  RequirementRepr &getRequirement() const;

  // Evaluation.
  llvm::Expected<Requirement> evaluate(Evaluator &evaluator,
                                       WhereClauseOwner,
                                       unsigned index,
                                       TypeResolutionStage stage) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const;
  Optional<Requirement> getCachedResult() const;
  void cacheResult(Requirement value) const;
};

/// Generate the USR for the given declaration.
class USRGenerationRequest :
    public SimpleRequest<USRGenerationRequest,
                         CacheKind::Cached,
                         std::string,
                         const ValueDecl*>
{
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<std::string> evaluate(Evaluator &eval, const ValueDecl* d) const;

public:
  // Cycle handling
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Caching
  bool isCached() const { return true; }
};

/// The zone number for the type checker.
#define SWIFT_TYPE_CHECKER_REQUESTS_TYPEID_ZONE 10

#define SWIFT_TYPEID_ZONE SWIFT_TYPE_CHECKER_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/TypeCheckerTypeIDZone.def"
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
#include "swift/AST/TypeCheckerTypeIDZone.def"
#undef SWIFT_TYPEID

} // end namespace swift

#endif // SWIFT_TYPE_CHECK_REQUESTS_H
