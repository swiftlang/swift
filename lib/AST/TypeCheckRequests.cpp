//===--- TypeCheckRequests.cpp - Type Checking Requests ------------------===//
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
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"

using namespace swift;

namespace swift {
// Implement the type checker type zone (zone 10).
#define SWIFT_TYPEID_ZONE SWIFT_TYPE_CHECKER_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/TypeCheckerTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

void swift::simple_display(
       llvm::raw_ostream &out,
       const llvm::PointerUnion<TypeDecl *, ExtensionDecl *> &value) {
  if (auto type = value.dyn_cast<TypeDecl *>()) {
    type->dumpRef(out);
    return;
  }

  auto ext = value.get<ExtensionDecl *>();
  simple_display(out, ext);
}

void swift::simple_display(llvm::raw_ostream &out,
                           const TypeResolutionStage &value) {
  switch (value) {
  case TypeResolutionStage::Structural:
    out << "structural";
    break;

  case TypeResolutionStage::Interface:
    out << "interface";
    break;

  case TypeResolutionStage::Contextual:
    out << "contextual";
    break;
  }
}

//----------------------------------------------------------------------------//
// Inherited type computation.
//----------------------------------------------------------------------------//

TypeLoc &InheritedTypeRequest::getTypeLoc(
                        llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                        unsigned index) const {
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
    return typeDecl->getInherited()[index];

  return decl.get<ExtensionDecl *>()->getInherited()[index];
}

void InheritedTypeRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  diags.diagnose(typeLoc.getLoc(), diag::circular_reference);
}

void InheritedTypeRequest::noteCycleStep(DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  diags.diagnose(typeLoc.getLoc(), diag::circular_reference_through);
}

bool InheritedTypeRequest::isCached() const {
  return std::get<2>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Type> InheritedTypeRequest::getCachedResult() const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  if (typeLoc.wasValidated())
    return typeLoc.getType();

  return None;
}

void InheritedTypeRequest::cacheResult(Type value) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  typeLoc.setType(value);
}

//----------------------------------------------------------------------------//
// Superclass computation.
//----------------------------------------------------------------------------//
void SuperclassTypeRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto nominalDecl = std::get<0>(getStorage());
  diags.diagnose(nominalDecl, diag::circular_class_inheritance,
                 nominalDecl->getName());
}

void SuperclassTypeRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto nominalDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(nominalDecl, diag::circular_reference_through);
}

bool SuperclassTypeRequest::isCached() const {
  return std::get<1>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Type> SuperclassTypeRequest::getCachedResult() const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    if (classDecl->LazySemanticInfo.Superclass.getInt())
      return classDecl->LazySemanticInfo.Superclass.getPointer();

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    if (protocolDecl->LazySemanticInfo.Superclass.getInt())
      return protocolDecl->LazySemanticInfo.Superclass.getPointer();

  return None;
}

void SuperclassTypeRequest::cacheResult(Type value) const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    classDecl->LazySemanticInfo.Superclass.setPointerAndInt(value, true);

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    protocolDecl->LazySemanticInfo.Superclass.setPointerAndInt(value, true);
}

//----------------------------------------------------------------------------//
// Enum raw type computation.
//----------------------------------------------------------------------------//
void EnumRawTypeRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto enumDecl = std::get<0>(getStorage());
  diags.diagnose(enumDecl, diag::circular_enum_inheritance, enumDecl->getName());
}

void EnumRawTypeRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto enumDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(enumDecl, diag::circular_reference_through);
}

bool EnumRawTypeRequest::isCached() const {
  return std::get<1>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Type> EnumRawTypeRequest::getCachedResult() const {
  auto enumDecl = std::get<0>(getStorage());
  if (enumDecl->LazySemanticInfo.RawType.getInt())
    return enumDecl->LazySemanticInfo.RawType.getPointer();

  return None;
}

void EnumRawTypeRequest::cacheResult(Type value) const {
  auto enumDecl = std::get<0>(getStorage());
  enumDecl->LazySemanticInfo.RawType.setPointerAndInt(value, true);
}

//----------------------------------------------------------------------------//
// Overridden decls computation.
//----------------------------------------------------------------------------//
void OverriddenDeclsRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_reference);
}

void OverriddenDeclsRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_reference_through);
}

//----------------------------------------------------------------------------//
// isObjC computation.
//----------------------------------------------------------------------------//

void IsObjCRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_reference);
}

void IsObjCRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(decl, diag::circular_reference_through);
}

Optional<bool> IsObjCRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  if (decl->LazySemanticInfo.isObjCComputed)
    return decl->LazySemanticInfo.isObjC;

  return None;
}

void IsObjCRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setIsObjC(value);
}

//----------------------------------------------------------------------------//
// isDynamic computation.
//----------------------------------------------------------------------------//

void IsDynamicRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_reference);
}

void IsDynamicRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(decl, diag::circular_reference_through);
}

Optional<bool> IsDynamicRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  if (decl->LazySemanticInfo.isDynamicComputed)
    return decl->LazySemanticInfo.isDynamic;

  return None;
}

void IsDynamicRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setIsDynamic(value);
}

//----------------------------------------------------------------------------//
// Requirement computation.
//----------------------------------------------------------------------------//

WhereClauseOwner::WhereClauseOwner(Decl *decl)
  : dc(decl->getInnermostDeclContext()), source(decl) { }

SourceLoc WhereClauseOwner::getLoc() const {
  if (auto decl = source.dyn_cast<Decl *>())
    return decl->getLoc();

  if (auto attr = source.dyn_cast<SpecializeAttr *>())
    return attr->getLocation();

  return source.get<GenericParamList *>()->getWhereLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           const WhereClauseOwner &owner) {
  if (auto decl = owner.source.dyn_cast<Decl *>()) {
    simple_display(out, decl);
  } else if (owner.source.is<SpecializeAttr *>()) {
    out << "@_specialize";
  } else {
    out << "(SIL generic parameter list)";
  }
}

void RequirementRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto owner = std::get<0>(getStorage());
  diags.diagnose(owner.getLoc(), diag::circular_reference);
}

void RequirementRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto owner = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(owner.getLoc(), diag::circular_reference_through);
}

MutableArrayRef<RequirementRepr>
RequirementRequest::getRequirements(WhereClauseOwner owner) {
  if (auto genericParams = owner.source.dyn_cast<GenericParamList *>()) {
    return genericParams->getRequirements();
  }

  if (auto attr = owner.source.dyn_cast<SpecializeAttr *>()) {
    if (auto whereClause = attr->getTrailingWhereClause())
      return whereClause->getRequirements();
    
    return { };
  }

  auto decl = owner.source.dyn_cast<Decl *>();
  if (!decl)
    return { };

  if (auto proto = dyn_cast<ProtocolDecl>(decl)) {
    if (auto whereClause = proto->getTrailingWhereClause())
      return whereClause->getRequirements();

    return { };
  }

  if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl)) {
    if (auto whereClause = assocType->getTrailingWhereClause())
      return whereClause->getRequirements();
  }

  if (auto genericContext = decl->getAsGenericContext()) {
    if (auto genericParams = genericContext->getGenericParams())
      return genericParams->getRequirements();
  }

  return { };
}

bool RequirementRequest::visitRequirements(
      WhereClauseOwner owner, TypeResolutionStage stage,
      llvm::function_ref<bool(Requirement, RequirementRepr*)> callback) {
  auto &evaluator = owner.dc->getASTContext().evaluator;
  auto requirements = getRequirements(owner);
  for (unsigned index : indices(requirements)) {
    // Resolve to a requirement.
    auto req = evaluator(RequirementRequest{owner, index, stage});
    if (req) {
      // Invoke the callback. If it returns true, we're done.
      if (callback(*req, &requirements[index]))
        return true;

      continue;
    }

    llvm::handleAllErrors(req.takeError(),
      [](const CyclicalRequestError<RequirementRequest> &E) {
        // cycle detected
      });
  }

  return false;
}

RequirementRepr &RequirementRequest::getRequirement() const {
  auto owner = std::get<0>(getStorage());
  auto index = std::get<1>(getStorage());
  return getRequirements(owner)[index];
}

bool RequirementRequest::isCached() const {
  return std::get<2>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Requirement> RequirementRequest::getCachedResult() const {
  auto &reqRepr = getRequirement();
  switch (reqRepr.getKind()) {
  case RequirementReprKind::TypeConstraint:
    if (!reqRepr.getSubjectLoc().wasValidated() ||
        !reqRepr.getConstraintLoc().wasValidated())
      return None;

    return Requirement(reqRepr.getConstraint()->getClassOrBoundGenericClass()
                         ? RequirementKind::Superclass
                         : RequirementKind::Conformance,
                       reqRepr.getSubject(),
                       reqRepr.getConstraint());

  case RequirementReprKind::SameType:
    if (!reqRepr.getFirstTypeLoc().wasValidated() ||
        !reqRepr.getSecondTypeLoc().wasValidated())
      return None;

    return Requirement(RequirementKind::SameType, reqRepr.getFirstType(),
                       reqRepr.getSecondType());

  case RequirementReprKind::LayoutConstraint:
    if (!reqRepr.getSubjectLoc().wasValidated())
      return None;

    return Requirement(RequirementKind::Layout, reqRepr.getSubject(),
                       reqRepr.getLayoutConstraint());
  }
  llvm_unreachable("unhandled kind");
}

void RequirementRequest::cacheResult(Requirement value) const {
  auto &reqRepr = getRequirement();
  switch (value.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
    reqRepr.getSubjectLoc().setType(value.getFirstType());
    reqRepr.getConstraintLoc().setType(value.getSecondType());
    break;

  case RequirementKind::SameType:
    reqRepr.getFirstTypeLoc().setType(value.getFirstType());
    reqRepr.getSecondTypeLoc().setType(value.getSecondType());
    break;

  case RequirementKind::Layout:
    reqRepr.getSubjectLoc().setType(value.getFirstType());
    reqRepr.getLayoutConstraintLoc()
      .setLayoutConstraint(value.getLayoutConstraint());
    break;
  }
}

//----------------------------------------------------------------------------//
// USR computation.
//----------------------------------------------------------------------------//

void USRGenerationRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &d = std::get<0>(storage);
  diags.diagnose(d, diag::circular_reference);
}

void USRGenerationRequest::noteCycleStep(DiagnosticEngine &diags) const {
  const auto &storage = getStorage();
  auto &d = std::get<0>(storage);
  diags.diagnose(d, diag::circular_reference);
}
