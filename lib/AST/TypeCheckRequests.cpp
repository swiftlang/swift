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
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"

using namespace swift;

namespace swift {
// Implement the type checker type zone (zone 10).
#define SWIFT_TYPEID_ZONE TypeChecker
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

void swift::simple_display(llvm::raw_ostream &out, Type type) {
  if (type)
    type.print(out);
  else
    out << "null";
}

void swift::simple_display(llvm::raw_ostream &out, const TypeRepr *TyR) {
  if (TyR)
    TyR->print(out);
  else
    out << "null";
}

void swift::simple_display(llvm::raw_ostream &out, const TypeLoc source) {
  out << "(";
  simple_display(out, source.getType());
  out << ", ";
  simple_display(out, source.getTypeRepr());
  out << ")";
}

//----------------------------------------------------------------------------//
// Inherited type computation.
//----------------------------------------------------------------------------//

SourceLoc InheritedTypeRequest::getNearestLoc() const {
  const auto &storage = getStorage();
  auto &typeLoc = getInheritedTypeLocAtIndex(std::get<0>(storage),
                                             std::get<1>(storage));
  return typeLoc.getLoc();
}

bool InheritedTypeRequest::isCached() const {
  return std::get<2>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Type> InheritedTypeRequest::getCachedResult() const {
  const auto &storage = getStorage();
  auto &typeLoc = getInheritedTypeLocAtIndex(std::get<0>(storage),
                                             std::get<1>(storage));
  if (typeLoc.wasValidated())
    return typeLoc.getType();

  return None;
}

void InheritedTypeRequest::cacheResult(Type value) const {
  const auto &storage = getStorage();
  auto &typeLoc = getInheritedTypeLocAtIndex(std::get<0>(storage),
                                             std::get<1>(storage));
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

bool SuperclassTypeRequest::isCached() const {
  return std::get<1>(getStorage()) == TypeResolutionStage::Interface;
}

Optional<Type> SuperclassTypeRequest::getCachedResult() const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    if (classDecl->LazySemanticInfo.SuperclassType.getInt())
      return classDecl->LazySemanticInfo.SuperclassType.getPointer();

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    if (protocolDecl->LazySemanticInfo.SuperclassType.getInt())
      return protocolDecl->LazySemanticInfo.SuperclassType.getPointer();

  return None;
}

void SuperclassTypeRequest::cacheResult(Type value) const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    classDecl->LazySemanticInfo.SuperclassType.setPointerAndInt(value, true);

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    protocolDecl->LazySemanticInfo.SuperclassType.setPointerAndInt(value, true);
}

//----------------------------------------------------------------------------//
// Enum raw type computation.
//----------------------------------------------------------------------------//
void EnumRawTypeRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto enumDecl = std::get<0>(getStorage());
  diags.diagnose(enumDecl, diag::circular_enum_inheritance, enumDecl->getName());
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
// isObjC computation.
//----------------------------------------------------------------------------//

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
// requiresClass computation.
//----------------------------------------------------------------------------//

void ProtocolRequiresClassRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_protocol_def, decl->getName());
}

void ProtocolRequiresClassRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto requirement = std::get<0>(getStorage());
  diags.diagnose(requirement, diag::kind_declname_declared_here,
                 DescriptiveDeclKind::Protocol,
                 requirement->getName());
}

Optional<bool> ProtocolRequiresClassRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  return decl->getCachedRequiresClass();
}

void ProtocolRequiresClassRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setCachedRequiresClass(value);
}

//----------------------------------------------------------------------------//
// existentialConformsToSelf computation.
//----------------------------------------------------------------------------//

void ExistentialConformsToSelfRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_protocol_def, decl->getName());
}

void ExistentialConformsToSelfRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto requirement = std::get<0>(getStorage());
  diags.diagnose(requirement, diag::kind_declname_declared_here,
                 DescriptiveDeclKind::Protocol, requirement->getName());
}

Optional<bool> ExistentialConformsToSelfRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  return decl->getCachedExistentialConformsToSelf();
}

void ExistentialConformsToSelfRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setCachedExistentialConformsToSelf(value);
}

//----------------------------------------------------------------------------//
// existentialTypeSupported computation.
//----------------------------------------------------------------------------//

void ExistentialTypeSupportedRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  auto decl = std::get<0>(getStorage());
  diags.diagnose(decl, diag::circular_protocol_def, decl->getName());
}

void ExistentialTypeSupportedRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto requirement = std::get<0>(getStorage());
  diags.diagnose(requirement, diag::kind_declname_declared_here,
                 DescriptiveDeclKind::Protocol, requirement->getName());
}

Optional<bool> ExistentialTypeSupportedRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  return decl->getCachedExistentialTypeSupported();
}

void ExistentialTypeSupportedRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setCachedExistentialTypeSupported(value);
}

//----------------------------------------------------------------------------//
// isFinal computation.
//----------------------------------------------------------------------------//

Optional<bool> IsFinalRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  if (decl->LazySemanticInfo.isFinalComputed)
    return decl->LazySemanticInfo.isFinal;

  return None;
}

void IsFinalRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->LazySemanticInfo.isFinalComputed = true;
  decl->LazySemanticInfo.isFinal = value;

  // Add an attribute for printing
  if (value && !decl->getAttrs().hasAttribute<FinalAttr>())
    decl->getAttrs().add(new (decl->getASTContext()) FinalAttr(/*Implicit=*/true));
}

//----------------------------------------------------------------------------//
// isDynamic computation.
//----------------------------------------------------------------------------//

Optional<bool> IsDynamicRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  if (decl->LazySemanticInfo.isDynamicComputed)
    return decl->LazySemanticInfo.isDynamic;

  return None;
}

void IsDynamicRequest::cacheResult(bool value) const {
  auto decl = std::get<0>(getStorage());
  decl->setIsDynamic(value);

  // Add an attribute for printing
  if (value && !decl->getAttrs().hasAttribute<DynamicAttr>())
    decl->getAttrs().add(new (decl->getASTContext()) DynamicAttr(/*Implicit=*/true));
}

//----------------------------------------------------------------------------//
// RequirementSignatureRequest computation.
//----------------------------------------------------------------------------//

Optional<ArrayRef<Requirement>> RequirementSignatureRequest::getCachedResult() const {
  auto proto = std::get<0>(getStorage());
  if (proto->isRequirementSignatureComputed())
    return proto->getCachedRequirementSignature();

  return None;
}

void RequirementSignatureRequest::cacheResult(ArrayRef<Requirement> value) const {
  auto proto = std::get<0>(getStorage());
  proto->setRequirementSignature(value);
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

SourceLoc RequirementRequest::getNearestLoc() const {
  auto owner = std::get<0>(getStorage());
  return owner.getLoc();
}

MutableArrayRef<RequirementRepr> WhereClauseOwner::getRequirements() const {
  if (auto genericParams = source.dyn_cast<GenericParamList *>()) {
    return genericParams->getRequirements();
  }

  if (auto attr = source.dyn_cast<SpecializeAttr *>()) {
    if (auto whereClause = attr->getTrailingWhereClause())
      return whereClause->getRequirements();
    
    return { };
  }

  auto decl = source.dyn_cast<Decl *>();
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

bool WhereClauseOwner::visitRequirements(
    TypeResolutionStage stage,
    llvm::function_ref<bool(Requirement, RequirementRepr *)> callback)
    const && {
  auto &evaluator = dc->getASTContext().evaluator;
  auto requirements = getRequirements();
  for (unsigned index : indices(requirements)) {
    // Resolve to a requirement.
    auto req = evaluator(RequirementRequest{*this, index, stage});
    if (req) {
      // Invoke the callback. If it returns true, we're done.
      if (callback(*req, &requirements[index]))
        return true;

      continue;
    }

    llvm::handleAllErrors(
        req.takeError(), [](const CyclicalRequestError<RequirementRequest> &E) {
          // cycle detected
        });
  }

  return false;
}

RequirementRepr &RequirementRequest::getRequirement() const {
  auto owner = std::get<0>(getStorage());
  auto index = std::get<1>(getStorage());
  return owner.getRequirements()[index];
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
// DefaultTypeRequest.
//----------------------------------------------------------------------------//

void swift::simple_display(llvm::raw_ostream &out,
                           const KnownProtocolKind kind) {
  out << getProtocolName(kind);
}

//----------------------------------------------------------------------------//
// DefaultTypeRequest caching.
//----------------------------------------------------------------------------//

Optional<Type> DefaultTypeRequest::getCachedResult() const {
  auto *DC = std::get<1>(getStorage());
  auto knownProtocolKind = std::get<0>(getStorage());
  const auto &cachedType = DC->getASTContext().getDefaultTypeRequestCache(
      DC->getParentSourceFile(), knownProtocolKind);
  return cachedType ? Optional<Type>(cachedType) : None;
}

void DefaultTypeRequest::cacheResult(Type value) const {
  auto *DC = std::get<1>(getStorage());
  auto knownProtocolKind = std::get<0>(getStorage());
  auto &cacheEntry = DC->getASTContext().getDefaultTypeRequestCache(
                         DC->getParentSourceFile(), knownProtocolKind);
  cacheEntry = value;
}

bool PropertyWrapperTypeInfoRequest::isCached() const {
  auto nominal = std::get<0>(getStorage());
  return nominal->getAttrs().hasAttribute<PropertyWrapperAttr>();;
}

bool AttachedPropertyWrappersRequest::isCached() const {
  auto var = std::get<0>(getStorage());
  return !var->getAttrs().isEmpty();
}

bool AttachedPropertyWrapperTypeRequest::isCached() const {
  auto var = std::get<0>(getStorage());
  return !var->getAttrs().isEmpty();
}

bool PropertyWrapperBackingPropertyTypeRequest::isCached() const {
  auto var = std::get<0>(getStorage());
  return !var->getAttrs().isEmpty();
}

bool PropertyWrapperBackingPropertyInfoRequest::isCached() const {
  auto var = std::get<0>(getStorage());
  return !var->getAttrs().isEmpty();
}

bool PropertyWrapperMutabilityRequest::isCached() const {
  auto var = std::get<0>(getStorage());
  return !var->getAttrs().isEmpty();
}

void swift::simple_display(
    llvm::raw_ostream &out, const PropertyWrapperTypeInfo &propertyWrapper) {
  out << "{ ";
  if (propertyWrapper.valueVar)
    out << propertyWrapper.valueVar->printRef();
  else
    out << "null";
  out << ", ";
  if (propertyWrapper.wrappedValueInit)
    out << propertyWrapper.wrappedValueInit->printRef();
  else
    out << "null";
  out << " }";
}

void swift::simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperBackingPropertyInfo &backingInfo) {
  out << "{ ";
  if (backingInfo.backingVar)
    backingInfo.backingVar->dumpRef(out);
  out << " }";
}

void swift::simple_display(
  llvm::raw_ostream &out, const CtorInitializerKind initKind) {
  out << "{ ";
  switch (initKind) {
  case CtorInitializerKind::Designated:
    out << "designated"; break;
  case CtorInitializerKind::Convenience:
    out << "convenience"; break;
  case CtorInitializerKind::ConvenienceFactory:
    out << "convenience_factory"; break;
  case CtorInitializerKind::Factory:
    out << "factory"; break;
  }
  out << " }";
}

void swift::simple_display(llvm::raw_ostream &os, PropertyWrapperMutability m) {
  static const char *names[] =
    {"is nonmutating", "is mutating", "doesn't exist"};
  
  os << "getter " << names[m.Getter] << ", setter " << names[m.Setter];
}

void swift::simple_display(llvm::raw_ostream &out,
                           const ResilienceExpansion &value) {
  out << value;
}

//----------------------------------------------------------------------------//
// FunctionBuilder-related requests.
//----------------------------------------------------------------------------//

bool AttachedFunctionBuilderRequest::isCached() const {
  // Only needs to be cached if there are any custom attributes.
  auto var = std::get<0>(getStorage());
  return var->getAttrs().hasAttribute<CustomAttr>();
}

//----------------------------------------------------------------------------//
// SelfAccessKindRequest computation.
//----------------------------------------------------------------------------//

Optional<SelfAccessKind> SelfAccessKindRequest::getCachedResult() const {
  auto *funcDecl = std::get<0>(getStorage());
  return funcDecl->getCachedSelfAccessKind();
}

void SelfAccessKindRequest::cacheResult(SelfAccessKind value) const {
  auto *funcDecl = std::get<0>(getStorage());
  funcDecl->setSelfAccessKind(value);
}

//----------------------------------------------------------------------------//
// IsGetterMutatingRequest computation.
//----------------------------------------------------------------------------//

Optional<bool> IsGetterMutatingRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.IsGetterMutatingComputed)
    return storage->LazySemanticInfo.IsGetterMutating;
  return None;
}

void IsGetterMutatingRequest::cacheResult(bool value) const {
  auto *storage = std::get<0>(getStorage());
  storage->setIsGetterMutating(value);
}

//----------------------------------------------------------------------------//
// IsSetterMutatingRequest computation.
//----------------------------------------------------------------------------//

Optional<bool> IsSetterMutatingRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.IsSetterMutatingComputed)
    return storage->LazySemanticInfo.IsSetterMutating;
  return None;
}

void IsSetterMutatingRequest::cacheResult(bool value) const {
  auto *storage = std::get<0>(getStorage());
  storage->setIsSetterMutating(value);
}

//----------------------------------------------------------------------------//
// OpaqueReadOwnershipRequest computation.
//----------------------------------------------------------------------------//

Optional<OpaqueReadOwnership>
OpaqueReadOwnershipRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.OpaqueReadOwnershipComputed)
    return OpaqueReadOwnership(storage->LazySemanticInfo.OpaqueReadOwnership);
  return None;
}

void OpaqueReadOwnershipRequest::cacheResult(OpaqueReadOwnership value) const {
  auto *storage = std::get<0>(getStorage());
  storage->setOpaqueReadOwnership(value);
}

//----------------------------------------------------------------------------//
// StorageImplInfoRequest computation.
//----------------------------------------------------------------------------//

Optional<StorageImplInfo>
StorageImplInfoRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.ImplInfoComputed)
    return storage->ImplInfo;
  return None;
}

void StorageImplInfoRequest::cacheResult(StorageImplInfo value) const {
  auto *storage = std::get<0>(getStorage());
  storage->setImplInfo(value);
}

//----------------------------------------------------------------------------//
// RequiresOpaqueAccessorsRequest computation.
//----------------------------------------------------------------------------//

Optional<bool>
RequiresOpaqueAccessorsRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.RequiresOpaqueAccessorsComputed)
    return storage->LazySemanticInfo.RequiresOpaqueAccessors;
  return None;
}

void RequiresOpaqueAccessorsRequest::cacheResult(bool value) const {
  auto *storage = std::get<0>(getStorage());
  storage->LazySemanticInfo.RequiresOpaqueAccessorsComputed = 1;
  storage->LazySemanticInfo.RequiresOpaqueAccessors = value;
}

//----------------------------------------------------------------------------//
// RequiresOpaqueModifyCoroutineRequest computation.
//----------------------------------------------------------------------------//

Optional<bool>
RequiresOpaqueModifyCoroutineRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  if (storage->LazySemanticInfo.RequiresOpaqueModifyCoroutineComputed)
    return storage->LazySemanticInfo.RequiresOpaqueModifyCoroutine;
  return None;
}

void RequiresOpaqueModifyCoroutineRequest::cacheResult(bool value) const {
  auto *storage = std::get<0>(getStorage());
  storage->LazySemanticInfo.RequiresOpaqueModifyCoroutineComputed = 1;
  storage->LazySemanticInfo.RequiresOpaqueModifyCoroutine = value;
}

//----------------------------------------------------------------------------//
// IsAccessorTransparentRequest computation.
//----------------------------------------------------------------------------//

Optional<bool>
IsAccessorTransparentRequest::getCachedResult() const {
  auto *accessor = std::get<0>(getStorage());
  return accessor->getCachedIsTransparent();
}

void IsAccessorTransparentRequest::cacheResult(bool value) const {
  auto *accessor = std::get<0>(getStorage());
  accessor->setIsTransparent(value);

  // For interface printing, API diff, etc.
  if (value) {
    auto &attrs = accessor->getAttrs();
    if (!attrs.hasAttribute<TransparentAttr>()) {
      auto &ctx = accessor->getASTContext();
      attrs.add(new (ctx) TransparentAttr(/*IsImplicit=*/true));
    }
  }
}

//----------------------------------------------------------------------------//
// SynthesizeAccessorRequest computation.
//----------------------------------------------------------------------------//

Optional<AccessorDecl *>
SynthesizeAccessorRequest::getCachedResult() const {
  auto *storage = std::get<0>(getStorage());
  auto kind = std::get<1>(getStorage());
  auto *accessor = storage->getAccessor(kind);
  if (accessor)
    return accessor;
  return None;
}

void SynthesizeAccessorRequest::cacheResult(AccessorDecl *accessor) const {
  auto *storage = std::get<0>(getStorage());
  auto kind = std::get<1>(getStorage());

  storage->setSynthesizedAccessor(kind, accessor);
}

//----------------------------------------------------------------------------//
// EmittedMembersRequest computation.
//----------------------------------------------------------------------------//

Optional<DeclRange>
EmittedMembersRequest::getCachedResult() const {
  auto *classDecl = std::get<0>(getStorage());
  if (classDecl->hasForcedEmittedMembers())
    return classDecl->getMembers();
  return None;
}

void EmittedMembersRequest::cacheResult(DeclRange result) const {
  auto *classDecl = std::get<0>(getStorage());
  classDecl->setHasForcedEmittedMembers();
}

//----------------------------------------------------------------------------//
// IsImplicitlyUnwrappedOptionalRequest computation.
//----------------------------------------------------------------------------//

Optional<bool>
IsImplicitlyUnwrappedOptionalRequest::getCachedResult() const {
  auto *decl = std::get<0>(getStorage());
  if (decl->LazySemanticInfo.isIUOComputed)
    return decl->LazySemanticInfo.isIUO;
  return None;
}

void IsImplicitlyUnwrappedOptionalRequest::cacheResult(bool value) const {
  auto *decl = std::get<0>(getStorage());
  decl->setImplicitlyUnwrappedOptional(value);
}
