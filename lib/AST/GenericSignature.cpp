//===--- GenericSignature.cpp - Generic Signature AST ---------------------===//
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
// This file implements the GenericSignature class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericSignature.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/STLExtras.h"
#include "RequirementMachine/RequirementMachine.h"
#include <functional>

using namespace swift;

void ConformancePath::print(raw_ostream &out) const {
  llvm::interleave(
      begin(), end(),
      [&](const Entry &entry) {
        entry.first.print(out);
        out << ": " << entry.second->getName();
      },
      [&] { out << " -> "; });
}

void ConformancePath::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

GenericSignatureImpl::GenericSignatureImpl(
    ArrayRef<GenericTypeParamType *> params,
    ArrayRef<Requirement> requirements, bool isKnownCanonical)
    : NumGenericParams(params.size()), NumRequirements(requirements.size()),
      CanonicalSignatureOrASTContext() {
  std::uninitialized_copy(params.begin(), params.end(),
                          getTrailingObjects<GenericTypeParamType *>());
  std::uninitialized_copy(requirements.begin(), requirements.end(),
                          getTrailingObjects<Requirement>());

#ifndef NDEBUG
  // Make sure generic parameters are in the right order, and
  // none are missing.
  unsigned depth = 0;
  unsigned count = 0;
  for (auto param : params) {
    if (param->getDepth() != depth) {
      assert(param->getDepth() > depth && "Generic parameter depth mismatch");
      depth = param->getDepth();
      count = 0;
    }
    assert(param->getIndex() == count && "Generic parameter index mismatch");
    ++count;
  }
#endif

  if (isKnownCanonical)
    CanonicalSignatureOrASTContext =
        &GenericSignature::getASTContext(params, requirements);
}

ArrayRef<GenericTypeParamType *>
GenericSignatureImpl::getInnermostGenericParams() const {
  const auto params = getGenericParams();

  const unsigned maxDepth = getMaxDepth();
  if (params.front()->getDepth() == maxDepth)
    return params;

  // There is a depth change. Count the number of elements
  // to slice off the front.
  unsigned sliceCount = params.size() - 1;
  while (true) {
    if (params[sliceCount - 1]->getDepth() != maxDepth)
      break;
    --sliceCount;
  }

  return params.slice(sliceCount);
}

unsigned GenericSignatureImpl::getMaxDepth() const {
  return getGenericParams().back()->getDepth();
}

unsigned GenericSignature::getNextDepth() const {
  if (!getPointer())
    return 0;
  return getPointer()->getMaxDepth() + 1;
}

void GenericSignatureImpl::forEachParam(
    llvm::function_ref<void(GenericTypeParamType *, bool)> callback) const {
  // Figure out which generic parameters are concrete or same-typed to another
  // type parameter.
  auto genericParams = getGenericParams();
  auto genericParamsAreCanonical =
    SmallVector<bool, 4>(genericParams.size(), true);

  for (auto req : getRequirements()) {
    GenericTypeParamType *gp;
    bool isCanonical = false;
    switch (req.getKind()) {
    case RequirementKind::SameType: {
      if (req.getSecondType()->isParameterPack() != 
          req.getFirstType()->isParameterPack()) {
        // This is a same-element requirement, which does not make
        // type parameters non-canonical.
        isCanonical = true;
      }

      if (auto secondGP = req.getSecondType()->getAs<GenericTypeParamType>()) {
        // If two generic parameters are same-typed, then the right-hand one
        // is non-canonical.
        assert(req.getFirstType()->is<GenericTypeParamType>());
        gp = secondGP;
      } else {
        // Otherwise, the right-hand side is an associated type or concrete
        // type, and the left-hand one is non-canonical.
        gp = req.getFirstType()->getAs<GenericTypeParamType>();
        if (!gp)
          continue;

        // If an associated type is same-typed, it doesn't constrain the generic
        // parameter itself. That is, if T == U.Foo, then T is canonical,
        // whereas U.Foo is not.
        if (req.getSecondType()->isTypeParameter())
          continue;
      }
      break;
    }

    case RequirementKind::Superclass:
    case RequirementKind::Conformance:
    case RequirementKind::Layout:
    case RequirementKind::SameShape:
      continue;
    }

    unsigned index = GenericParamKey(gp).findIndexIn(genericParams);
    genericParamsAreCanonical[index] = isCanonical;
  }

  // Call the callback with each parameter and the result of the above analysis.
  for (auto index : indices(genericParams))
    callback(genericParams[index], genericParamsAreCanonical[index]);
}

bool GenericSignatureImpl::areAllParamsConcrete() const {
  unsigned numConcreteGenericParams = 0;
  for (const auto &req : getRequirements()) {
    switch (req.getKind()) {
    case RequirementKind::SameType:
      if (!req.getFirstType()->is<GenericTypeParamType>())
        continue;
      if (req.getSecondType()->isTypeParameter())
        continue;

      ++numConcreteGenericParams;
      break;

    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::Layout:
    case RequirementKind::SameShape:
      continue;
    }
  }

  return numConcreteGenericParams == getGenericParams().size();
}

bool GenericSignatureImpl::hasParameterPack() const {
  for (auto *paramTy : getGenericParams()) {
    if (paramTy->isParameterPack())
      return true;
  }

  return false;
}

ASTContext &GenericSignature::getASTContext(
                                    ArrayRef<GenericTypeParamType *> params,
                                    ArrayRef<swift::Requirement> requirements) {
  // The params and requirements cannot both be empty.
  if (!params.empty())
    return params.front()->getASTContext();
  else
    return requirements.front().getFirstType()->getASTContext();
}

/// Retrieve the generic parameters.
ArrayRef<GenericTypeParamType *> GenericSignature::getGenericParams() const {
  return isNull()
      ? ArrayRef<GenericTypeParamType *>()
      : getPointer()->getGenericParams();
}

/// Retrieve the innermost generic parameters.
///
/// Given a generic signature for a nested generic type, produce an
/// array of the generic parameters for the innermost generic type.
ArrayRef<GenericTypeParamType *> GenericSignature::getInnermostGenericParams() const {
  return isNull()
      ? ArrayRef<GenericTypeParamType *>()
      : getPointer()->getInnermostGenericParams();
}

/// Retrieve the requirements.
ArrayRef<Requirement> GenericSignature::getRequirements() const {
  return isNull()
      ? ArrayRef<Requirement>{}
      : getPointer()->getRequirements();
}

rewriting::RequirementMachine *
GenericSignatureImpl::getRequirementMachine() const {
  if (Machine)
    return Machine;

  const_cast<GenericSignatureImpl *>(this)->Machine
      = getASTContext().getRewriteContext().getRequirementMachine(
          getCanonicalSignature());
  return Machine;
}

bool GenericSignatureImpl::isEqual(GenericSignature Other) const {
  return getCanonicalSignature() == Other.getCanonicalSignature();
}

bool GenericSignatureImpl::isCanonical() const {
  if (CanonicalSignatureOrASTContext.is<ASTContext *>())
    return true;
  return getCanonicalSignature().getPointer() == this;
}

CanGenericSignature
CanGenericSignature::getCanonical(ArrayRef<GenericTypeParamType *> params,
                                  ArrayRef<Requirement> requirements) {
  // Canonicalize the parameters and requirements.
  SmallVector<GenericTypeParamType*, 8> canonicalParams;
  canonicalParams.reserve(params.size());
  for (auto param : params) {
    canonicalParams.push_back(cast<GenericTypeParamType>(param->getCanonicalType()));
  }

  SmallVector<Requirement, 8> canonicalRequirements;
  canonicalRequirements.reserve(requirements.size());
  for (auto &reqt : requirements)
    canonicalRequirements.push_back(reqt.getCanonical());

  auto canSig = get(canonicalParams, canonicalRequirements,
                    /*isKnownCanonical=*/true);

  return CanGenericSignature(canSig);
}

CanGenericSignature GenericSignature::getCanonicalSignature() const {
  // If the underlying pointer is null, return `CanGenericSignature()`.
  if (isNull())
    return CanGenericSignature();
  // Otherwise, return the canonical signature of the underlying pointer.
  return getPointer()->getCanonicalSignature();
}

CanGenericSignature GenericSignatureImpl::getCanonicalSignature() const {
  // If we haven't computed the canonical signature yet, do so now.
  if (CanonicalSignatureOrASTContext.isNull()) {
    // Compute the canonical signature.
    auto canSig = CanGenericSignature::getCanonical(getGenericParams(),
                                                    getRequirements());

    // Record either the canonical signature or an indication that
    // this is the canonical signature.
    if (canSig.getPointer() != this)
      CanonicalSignatureOrASTContext = canSig.getPointer();
    else
      CanonicalSignatureOrASTContext = &getGenericParams()[0]->getASTContext();

    // Return the canonical signature.
    return canSig;
  }

  // A stored ASTContext indicates that this is the canonical
  // signature.
  if (CanonicalSignatureOrASTContext.is<ASTContext *>())
    return CanGenericSignature(this);

  // Otherwise, return the stored canonical signature.
  return CanGenericSignature(
      CanonicalSignatureOrASTContext.get<const GenericSignatureImpl *>());
}

GenericEnvironment *GenericSignature::getGenericEnvironment() const {
  if (isNull())
    return nullptr;
  return getPointer()->getGenericEnvironment();
}

GenericEnvironment *GenericSignatureImpl::getGenericEnvironment() const {
  if (GenericEnv == nullptr) {
    const auto impl = const_cast<GenericSignatureImpl *>(this);
    impl->GenericEnv = GenericEnvironment::forPrimary(this);
  }

  return GenericEnv;
}

GenericSignature::LocalRequirements
GenericSignatureImpl::getLocalRequirements(Type depType) const {
  assert(depType->isTypeParameter() && "Expected a type parameter here");

  return getRequirementMachine()->getLocalRequirements(depType);
}

ASTContext &GenericSignatureImpl::getASTContext() const {
  // Canonical signatures store the ASTContext directly.
  if (auto ctx = CanonicalSignatureOrASTContext.dyn_cast<ASTContext *>())
    return *ctx;

  // For everything else, just get it from the generic parameter.
  return GenericSignature::getASTContext(getGenericParams(), getRequirements());
}

bool GenericSignatureImpl::requiresClass(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have superclass requirements");

  return getRequirementMachine()->requiresClass(type);
}

/// Determine the superclass bound on the given dependent type.
Type GenericSignatureImpl::getSuperclassBound(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have superclass requirements");

  return getRequirementMachine()->getSuperclassBound(
      type, getGenericParams());
}

/// Determine the set of protocols to which the given type parameter is
/// required to conform.
GenericSignature::RequiredProtocols
GenericSignatureImpl::getRequiredProtocols(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  return getRequirementMachine()->getRequiredProtocols(type);
}

bool GenericSignatureImpl::requiresProtocol(Type type,
                                            ProtocolDecl *proto) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  return getRequirementMachine()->requiresProtocol(type, proto);
}

/// Determine whether the given dependent type is equal to a concrete type.
bool GenericSignatureImpl::isConcreteType(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  return getRequirementMachine()->isConcreteType(type);
}

/// Return the concrete type that the given type parameter is constrained to,
/// or the null Type if it is not the subject of a concrete same-type
/// constraint.
Type GenericSignatureImpl::getConcreteType(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  return getRequirementMachine()->getConcreteType(type, getGenericParams());
}

LayoutConstraint GenericSignatureImpl::getLayoutConstraint(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have layout constraints");

  return getRequirementMachine()->getLayoutConstraint(type);
}

bool GenericSignatureImpl::areReducedTypeParametersEqual(Type type1,
                                                         Type type2) const {
  assert(type1->isTypeParameter());
  assert(type2->isTypeParameter());

  if (type1.getPointer() == type2.getPointer())
    return true;

  return getRequirementMachine()->areReducedTypeParametersEqual(type1, type2);
}

bool GenericSignatureImpl::isRequirementSatisfied(
    Requirement requirement,
    bool allowMissing,
    bool brokenPackBehavior) const {
  if (requirement.getFirstType()->hasTypeParameter()) {
    auto *genericEnv = getGenericEnvironment();

    if (brokenPackBehavior) {
      // Swift 5.9 shipped with a bug here where this method would return
      // incorrect results. Maintain the old behavior specifically for two
      // call sites in the ASTMangler.
      if ((requirement.getKind() == RequirementKind::SameType ||
           requirement.getKind() == RequirementKind::Superclass) &&
          !requirement.getSecondType()->isTypeParameter() &&
          requirement.getSecondType().findIf([&](Type t) -> bool {
            return t->is<PackExpansionType>();
          })) {
        return false;
      }
    }

    requirement = requirement.subst(
        QueryInterfaceTypeSubstitutions{genericEnv},
        LookUpConformanceInModule(),
        SubstFlags::PreservePackExpansionLevel);
  }

  SmallVector<Requirement, 2> subReqs;
  switch (requirement.checkRequirement(subReqs, allowMissing)) {
  case CheckRequirementResult::Success:
    return true;

  case CheckRequirementResult::ConditionalConformance:
    // FIXME: Need to check conditional requirements here.
    return true;

  case CheckRequirementResult::PackRequirement:
    // FIXME
    assert(false && "Refactor this");
    return true;

  case CheckRequirementResult::RequirementFailure:
  case CheckRequirementResult::SubstitutionFailure:
    return false;
  }
}

SmallVector<Requirement, 4>
GenericSignature::requirementsNotSatisfiedBy(GenericSignature otherSig) const {
  // The null generic signature has no requirements, therefore all requirements
  // are satisfied by any signature.
  if (isNull()) {
    return {};
  }
  return getPointer()->requirementsNotSatisfiedBy(otherSig);
}

SmallVector<Requirement, 4> GenericSignatureImpl::requirementsNotSatisfiedBy(
                                            GenericSignature otherSig) const {
  SmallVector<Requirement, 4> result;

  // If the signatures match by pointer, all requirements are satisfied.
  if (otherSig.getPointer() == this) return result;

  // If there is no other signature, no requirements are satisfied.
  if (!otherSig) {
    const auto reqs = getRequirements();
    result.append(reqs.begin(), reqs.end());
    return result;
  }

  // If the canonical signatures are equal, all requirements are satisfied.
  if (getCanonicalSignature() == otherSig->getCanonicalSignature())
    return result;

  // Find the requirements that aren't satisfied.
  for (const auto &req : getRequirements()) {
    if (!otherSig->isRequirementSatisfied(req))
      result.push_back(req);
  }

  return result;
}

bool GenericSignatureImpl::isReducedType(Type type) const {
  // If the type isn't canonical, it's not reduced.
  if (!type->isCanonical())
    return false;

  // A fully concrete canonical type is reduced.
  if (!type->hasTypeParameter())
    return true;

  return getRequirementMachine()->isReducedType(type);
}

CanType GenericSignature::getReducedType(Type type) const {
  // The null generic signature has no requirements so cannot influence the
  // structure of the can type computed here.
  if (isNull()) {
    return type->getCanonicalType();
  }
  return getPointer()->getReducedType(type);
}

CanType GenericSignatureImpl::getReducedType(Type type) const {
  type = type->getCanonicalType();

  // A fully concrete type is already reduced.
  if (!type->hasTypeParameter())
    return CanType(type);

  return getRequirementMachine()->getReducedType(
      type, { })->getCanonicalType();
}

CanType GenericSignatureImpl::getReducedTypeParameter(CanType type) const {
  return getRequirementMachine()->getReducedTypeParameter(
      type, { })->getCanonicalType();
}

bool GenericSignatureImpl::isValidTypeParameter(Type type) const {
  return getRequirementMachine()->isValidTypeParameter(type);
}

ArrayRef<CanTypeWrapper<GenericTypeParamType>>
CanGenericSignature::getGenericParams() const {
  auto params =
      this->GenericSignature::getGenericParams();
  auto base = reinterpret_cast<const CanTypeWrapper<GenericTypeParamType> *>(
                                                              params.data());
  return {base, params.size()};
}

ConformancePath
GenericSignatureImpl::getConformancePath(Type type,
                                         ProtocolDecl *protocol) const {
  return getRequirementMachine()->getConformancePath(type, protocol);
}

TypeDecl *
GenericSignatureImpl::lookupNestedType(Type type, Identifier name) const {
  assert(type->isTypeParameter());

  return getRequirementMachine()->lookupNestedType(type, name);
}

Type
GenericSignatureImpl::getReducedShape(Type type) const {
  return getRequirementMachine()->getReducedShape(type, getGenericParams());
}

bool
GenericSignatureImpl::haveSameShape(Type type1, Type type2) const {
  return getRequirementMachine()->haveSameShape(type1, type2);
}

llvm::SmallVector<CanType, 2> GenericSignatureImpl::getShapeClasses() const {
  llvm::SmallSetVector<CanType, 2> result;

  forEachParam([&](GenericTypeParamType *gp, bool canonical) {
    if (!canonical || !gp->isParameterPack())
      return;

    result.insert(getReducedShape(gp)->getCanonicalType());
  });

  return result.takeVector();
}

unsigned GenericParamKey::findIndexIn(
                      ArrayRef<GenericTypeParamType *> genericParams) const {
  // For depth 0, we have random access. We perform the extra checking so that
  // we can return
  if (Depth == 0 && Index < genericParams.size() &&
      genericParams[Index] == *this)
    return Index;

  // At other depths, perform a binary search.
  unsigned result =
      std::lower_bound(genericParams.begin(), genericParams.end(), *this,
                       Ordering())
        - genericParams.begin();
  if (result < genericParams.size() && genericParams[result] == *this)
    return result;

  // We didn't find the parameter we were looking for.
  return genericParams.size();
}

SubstitutionMap GenericSignatureImpl::getIdentitySubstitutionMap() const {
  return SubstitutionMap::get(const_cast<GenericSignatureImpl *>(this),
    [](SubstitutableType *t) -> Type {
      auto param = cast<GenericTypeParamType>(t);
      if (!param->isParameterPack())
        return param;
      return PackType::getSingletonPackExpansion(param);
    },
    MakeAbstractConformanceForGenericType());
}

GenericTypeParamType *GenericSignatureImpl::getSugaredType(
    GenericTypeParamType *type) const {
  unsigned ordinal = getGenericParamOrdinal(type);
  return getGenericParams()[ordinal];
}

Type GenericSignatureImpl::getSugaredType(Type type) const {
  if (!type->hasTypeParameter())
    return type;

  return type.transformRec([this](TypeBase *Ty) -> std::optional<Type> {
    if (auto GP = dyn_cast<GenericTypeParamType>(Ty)) {
      return Type(getSugaredType(GP));
    }
    return std::nullopt;
  });
}

unsigned GenericSignatureImpl::getGenericParamOrdinal(
    GenericTypeParamType *param) const {
  return GenericParamKey(param).findIndexIn(getGenericParams());
}

Type GenericSignatureImpl::getUpperBound(Type type,
                                         bool forExistentialSelf,
                                         bool includeParameterizedProtocols) const {
  assert(type->isTypeParameter());

  llvm::SmallVector<Type, 2> types;
  unsigned rootDepth = type->getRootGenericParam()->getDepth();

  auto accept = [forExistentialSelf, rootDepth](Type t) {
    if (!forExistentialSelf)
      return true;

    return !t.findIf([rootDepth](Type t) {
      if (auto *paramTy = t->getAs<GenericTypeParamType>())
        return (paramTy->getDepth() == rootDepth);
      return false;
    });
  };

  // We start with the assumption we'll add a '& AnyObject' member to our
  // composition, but we might clear this below.
  bool hasExplicitAnyObject = requiresClass(type);

  // Look for the most derived superclass that does not involve the type
  // being erased.
  Type superclass = getSuperclassBound(type);
  if (superclass) {
    do {
      superclass = getReducedType(superclass);
      if (accept(superclass))
        break;
    } while ((superclass = superclass->getSuperclass()));

    // If we're going to have a superclass, we can drop the '& AnyObject'.
    if (superclass) {
      types.push_back(getSugaredType(superclass));
      hasExplicitAnyObject = false;
    }
  }

  auto &ctx = getASTContext();

  // Record the absence of Copyable and Escapable conformance, but only if
  // we didn't have a superclass or require AnyObject.
  InvertibleProtocolSet inverses;

  if (!superclass && !hasExplicitAnyObject) {
    for (auto ip : InvertibleProtocolSet::allKnown()) {
      auto *kp = ctx.getProtocol(::getKnownProtocolKind(ip));
      if (!requiresProtocol(type, kp))
        inverses.insert(ip);
    }
  }

  for (auto *proto : getRequiredProtocols(type)) {
    // Don't add invertible protocols to the composition, because we recorded
    // their absence above.
    if (proto->getInvertibleProtocolKind())
      continue;

    if (proto->requiresClass())
      hasExplicitAnyObject = false;

    auto *baseType = proto->getDeclaredInterfaceType()->castTo<ProtocolType>();

    auto primaryAssocTypes = proto->getPrimaryAssociatedTypes();
    if (includeParameterizedProtocols && !primaryAssocTypes.empty()) {
      SmallVector<Type, 2> argTypes;

      // Attempt to recover same-type requirements on primary associated types.
      for (auto *assocType : primaryAssocTypes) {
        // For each primary associated type A of P, compute the reduced type
        // of T.[P]A.
        auto memberType = getReducedType(DependentMemberType::get(type, assocType));

        // If the reduced type is at a lower depth than the root generic
        // parameter of T, then it's constrained.
        if (accept(memberType)) {
          argTypes.push_back(getSugaredType(memberType));
        }
      }

      // If we have constrained all primary associated types, create a
      // parameterized protocol type. During code completion, we might call
      // `getExistentialType` (which calls this method) on a generic parameter
      // that doesn't have all parameters specified, e.g. to get a consise
      // description of the parameter type to the following function.
      //
      // func foo<P: Publisher>(p: P) where P.Failure == Never
      //
      // In that case just add the base type in the default branch below.
      if (argTypes.size() == primaryAssocTypes.size()) {
        types.push_back(ParameterizedProtocolType::get(
            getASTContext(), baseType, argTypes));
        continue;
      }
    }
    types.push_back(baseType);
  }

  return ProtocolCompositionType::get(ctx, types, inverses,
                                      hasExplicitAnyObject);
}

Type GenericSignatureImpl::getExistentialType(Type paramTy) const {
  auto upperBound = getUpperBound(paramTy,
                                  /*forExistentialSelf=*/true,
                                  /*includeParameterizedProtocols=*/true);
  if (upperBound->isConstraintType())
    return ExistentialType::get(upperBound);
  assert(upperBound->getClassOrBoundGenericClass());
  return upperBound;
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &id) const {
  return GenericSignature::Profile(id, getPointer()->getGenericParams(),
                                     getPointer()->getRequirements());
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &ID,
                               ArrayRef<GenericTypeParamType *> genericParams,
                               ArrayRef<Requirement> requirements) {
  return GenericSignatureImpl::Profile(ID, genericParams, requirements);
}

void swift::simple_display(raw_ostream &out, GenericSignature sig) {
  if (sig)
    sig->print(out);
  else
    out << "NULL";
}

/// Compare two associated types.
int swift::compareAssociatedTypes(AssociatedTypeDecl *assocType1,
                                  AssociatedTypeDecl *assocType2) {
  // - by name.
  if (int result = assocType1->getName().str().compare(
                                              assocType2->getName().str()))
    return result;

  // Prefer an associated type with no overrides (i.e., an anchor) to one
  // that has overrides.
  bool hasOverridden1 = !assocType1->getOverriddenDecls().empty();
  bool hasOverridden2 = !assocType2->getOverriddenDecls().empty();
  if (hasOverridden1 != hasOverridden2)
    return hasOverridden1 ? +1 : -1;

  // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
  auto proto1 = assocType1->getProtocol();
  auto proto2 = assocType2->getProtocol();
  if (int compareProtocols = TypeDecl::compare(proto1, proto2))
    return compareProtocols;

  // Error case: if we have two associated types with the same name in the
  // same protocol, just tie-break based on source location.
  if (assocType1 != assocType2) {
    auto &ctx = assocType1->getASTContext();
    return ctx.SourceMgr.isBeforeInBuffer(assocType1->getLoc(),
                                          assocType2->getLoc()) ? -1 : +1;
  }

  return 0;
}

/// Canonical ordering for type parameters.
int swift::compareDependentTypes(Type type1, Type type2) {
  // Fast-path check for equality.
  if (type1->isEqual(type2)) return 0;

  // Ordering is as follows:
  // - Generic params
  auto gp1 = type1->getAs<GenericTypeParamType>();
  auto gp2 = type2->getAs<GenericTypeParamType>();
  if (gp1 && gp2)
    return GenericParamKey(gp1) < GenericParamKey(gp2) ? -1 : +1;

  // A generic parameter is always ordered before a nested type.
  if (static_cast<bool>(gp1) != static_cast<bool>(gp2))
    return gp1 ? -1 : +1;

  // - Dependent members
  auto depMemTy1 = type1->castTo<DependentMemberType>();
  auto depMemTy2 = type2->castTo<DependentMemberType>();

  // - by base, so t_0_n.`P.T` < t_1_m.`P.T`
  if (int compareBases =
        compareDependentTypes(depMemTy1->getBase(), depMemTy2->getBase()))
    return compareBases;

  // - by name, so t_n_m.`P.T` < t_n_m.`P.U`
  if (int compareNames = depMemTy1->getName().str().compare(
                                                  depMemTy2->getName().str()))
    return compareNames;

  auto *assocType1 = depMemTy1->getAssocType();
  auto *assocType2 = depMemTy2->getAssocType();
  if (int result = compareAssociatedTypes(assocType1, assocType2))
    return result;

  return 0;
}

#pragma mark Generic signature verification

void GenericSignature::verify() const {
  verify(getRequirements());
}

void GenericSignature::verify(ArrayRef<Requirement> reqts) const {
  auto dumpAndAbort = [&]() {
    llvm::errs() << "All requirements:\n";
    for (auto reqt : reqts) {
      reqt.dump(llvm::errs());
      llvm::errs() << "\n";
    }
    getPointer()->getRequirementMachine()->dump(llvm::errs());
    abort();
  };

  auto canSig = getCanonicalSignature();

  PrettyStackTraceGenericSignature debugStack("checking", canSig);

  // We collect conformance requirements to check that they're minimal.
  llvm::SmallDenseMap<CanType, SmallVector<ProtocolDecl *, 2>, 2> conformances;

  // We collect same-type requirements to check that they're minimal.
  llvm::SmallDenseMap<CanType, SmallVector<Type, 2>, 2> sameTypeComponents;

  // Check that the requirements satisfy certain invariants.
  for (unsigned idx : indices(reqts)) {
    const auto &reqt = reqts[idx].getCanonical();

    // Left-hand side must be a canonical type parameter.
    if (reqt.getKind() != RequirementKind::SameType) {
      if (!reqt.getFirstType()->isTypeParameter()) {
        llvm::errs() << "Left-hand side must be a type parameter: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      if (!canSig->isReducedType(reqt.getFirstType())) {
        llvm::errs() << "Left-hand side is not reduced: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }
    }

    // Check canonicalization of requirement itself.
    switch (reqt.getKind()) {
    case RequirementKind::SameShape:
      if (!reqt.getFirstType()->is<GenericTypeParamType>()) {
        llvm::errs() << "Left hand side is not a generic parameter: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      if (!reqt.getFirstType()->isRootParameterPack()) {
        llvm::errs() << "Left hand side is not a parameter pack: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      if (!reqt.getSecondType()->is<GenericTypeParamType>()) {
        llvm::errs() << "Right hand side is not a generic parameter: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      if (!reqt.getSecondType()->isRootParameterPack()) {
        llvm::errs() << "Right hand side is not a parameter pack: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      break;
    case RequirementKind::Superclass:
      if (!canSig->isReducedType(reqt.getSecondType())) {
        llvm::errs() << "Right-hand side is not reduced: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }
      break;

    case RequirementKind::Layout:
      break;

    case RequirementKind::SameType: {
      auto hasReducedOrConcreteParent = [&](Type type) {
        if (auto *dmt = type->getAs<DependentMemberType>()) {
          return (canSig->isReducedType(dmt->getBase()) ||
                  canSig->isConcreteType(dmt->getBase()));
        }
        return type->is<GenericTypeParamType>();
      };

      auto firstType = reqt.getFirstType();
      auto secondType = reqt.getSecondType();

      auto canType = canSig->getReducedType(firstType);
      auto &component = sameTypeComponents[canType];

      if (!hasReducedOrConcreteParent(firstType)) {
        llvm::errs() << "Left hand side does not have a reduced parent: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }

      if (reqt.getSecondType()->isTypeParameter()) {
        if (!hasReducedOrConcreteParent(secondType)) {
          llvm::errs() << "Right hand side does not have a reduced parent: ";
          reqt.dump(llvm::errs());
          llvm::errs() << "\n";
          dumpAndAbort();
        }

        if (compareDependentTypes(firstType, secondType) >= 0) {
          llvm::errs() << "Out-of-order type parameters: ";
          reqt.dump(llvm::errs());
          llvm::errs() << "\n";
          dumpAndAbort();
        }

        if (component.empty()) {
          component.push_back(firstType);
        } else if (!component.back()->isEqual(firstType)) {
          llvm::errs() << "Same-type requirement within an equiv. class "
                       << "is out-of-order: ";
          reqt.dump(llvm::errs());
          llvm::errs() << "\n";
          dumpAndAbort();
        }

        component.push_back(secondType);
      } else {
        if (!canSig->isReducedType(secondType)) {
          llvm::errs() << "Right hand side is not reduced: ";
          reqt.dump(llvm::errs());
          llvm::errs() << "\n";
          dumpAndAbort();
        }

        if (component.empty()) {
          component.push_back(secondType);
        } else if (!component.back()->isEqual(secondType)) {
          llvm::errs() << "Inconsistent concrete requirement in equiv. class: ";
          reqt.dump(llvm::errs());
          llvm::errs() << "\n";
          dumpAndAbort();
        }
      }
      break;
    }

    case RequirementKind::Conformance:
      // Collect all conformance requirements on each type parameter.
      conformances[CanType(reqt.getFirstType())].push_back(
          reqt.getProtocolDecl());
      break;
    }

    // From here on, we're only interested in requirements beyond the first.
    if (idx == 0) continue;

    // Make sure that the left-hand sides are in nondecreasing order.
    const auto &prevReqt = reqts[idx-1];
    int compareLHS =
      compareDependentTypes(prevReqt.getFirstType(), reqt.getFirstType());
    if (compareLHS > 0) {
      llvm::errs() << "Out-of-order left-hand side: ";
      reqt.dump(llvm::errs());
      llvm::errs() << "\n";
      dumpAndAbort();
    }

    // If we have a concrete same-type requirement, we shouldn't have any
    // other requirements on the same type.
    if (reqt.getKind() == RequirementKind::SameType &&
        !reqt.getSecondType()->isTypeParameter()) {
      if (compareLHS >= 0) {
        llvm::errs() << "Concrete subject type should not have "
                     << "any other requirements: ";
        reqt.dump(llvm::errs());
        llvm::errs() << "\n";
        dumpAndAbort();
      }
    }

    if (prevReqt.compare(reqt) >= 0) {
      llvm::errs() << "Out-of-order requirement: ";
      reqt.dump(llvm::errs());
      llvm::errs() << "\n";
      dumpAndAbort();
    }
  }

  // Make sure we don't have redundant protocol conformance requirements.
  for (const auto &pair : conformances) {
    const auto &protos = pair.second;
    auto canonicalProtos = protos;

    // canonicalizeProtocols() will sort them and filter out any protocols that
    // are refined by other protocols in the list. It should be a no-op at this
    // point.
    ProtocolType::canonicalizeProtocols(canonicalProtos);

    if (protos.size() != canonicalProtos.size()) {
      llvm::errs() << "Redundant conformance requirements in signature "
                   << *this << ":\n";
      llvm::errs() << "Ours:\n";
      for (auto *proto : protos)
        llvm::errs() << "- " << proto->getName() << "\n";
      llvm::errs() << "Theirs:\n";
      for (auto *proto : canonicalProtos)
        llvm::errs() << "- " << proto->getName() << "\n";
      dumpAndAbort();
    }
    if (!std::equal(protos.begin(), protos.end(), canonicalProtos.begin())) {
      llvm::errs() << "Out-of-order conformance requirements\n";
      dumpAndAbort();
    }
  }

  // Check same-type components for consistency.
  for (const auto &pair : sameTypeComponents) {
    if (pair.second.front()->isTypeParameter() &&
        !canSig->isReducedType(pair.second.front())) {
      llvm::errs() << "Abstract same-type requirement involving concrete types\n";
      llvm::errs() << "Reduced type: " << pair.first << "\n";
      llvm::errs() << "Left hand side of first requirement: "
                   << pair.second.front() << "\n";
      dumpAndAbort();
    }
  }
}

static Type stripBoundDependentMemberTypes(Type t) {
  if (auto *depMemTy = t->getAs<DependentMemberType>()) {
    return DependentMemberType::get(
      stripBoundDependentMemberTypes(depMemTy->getBase()),
      depMemTy->getName());
  }

  return t;
}

static Requirement stripBoundDependentMemberTypes(Requirement req) {
  auto subjectType = stripBoundDependentMemberTypes(req.getFirstType());

  switch (req.getKind()) {
  case RequirementKind::SameShape:
    // Same-shape requirements do not involve dependent member types.
    return req;

  case RequirementKind::Conformance:
    return Requirement(RequirementKind::Conformance, subjectType,
                       req.getSecondType());

  case RequirementKind::Superclass:
  case RequirementKind::SameType:
    return Requirement(req.getKind(), subjectType,
                       req.getSecondType().transformRec([](Type t) -> std::optional<Type> {
                         if (t->isTypeParameter())
                           return stripBoundDependentMemberTypes(t);
                         return std::nullopt;
                       }));

  case RequirementKind::Layout:
    return Requirement(RequirementKind::Layout, subjectType,
                       req.getLayoutConstraint());
  }

  llvm_unreachable("Bad requirement kind");
}

void swift::validateGenericSignature(ASTContext &context,
                                     GenericSignature sig) {
  llvm::errs() << "Validating generic signature: ";
  sig->print(llvm::errs());
  llvm::errs() << "\n";

  // Try building a new signature having the same requirements.
  SmallVector<GenericTypeParamType *, 2> genericParams;
  for (auto *genericParam :  sig.getGenericParams())
    genericParams.push_back(genericParam);

  SmallVector<Requirement, 2> requirements;
  for (auto requirement : sig.getRequirements())
    requirements.push_back(stripBoundDependentMemberTypes(requirement));

  {
    PrettyStackTraceGenericSignature debugStack("verifying", sig);

    auto newSigWithError = evaluateOrDefault(
        context.evaluator,
        AbstractGenericSignatureRequest{
            nullptr,
            genericParams,
            requirements,
            /*allowInverses=*/false},
        GenericSignatureWithError());

    // If there were any errors, the signature was invalid.
    auto errorFlags = newSigWithError.getInt();
    if (errorFlags.contains(GenericSignatureErrorFlags::HasInvalidRequirements) ||
        errorFlags.contains(GenericSignatureErrorFlags::CompletionFailed)) {
      context.Diags.diagnose(SourceLoc(), diag::generic_signature_not_valid,
                             sig->getAsString());
    }

    auto newSig = newSigWithError.getPointer();

    // The new signature should be equal.
    if (!newSig->isEqual(sig)) {
      context.Diags.diagnose(SourceLoc(), diag::generic_signature_not_equal,
                             sig->getAsString(), newSig->getAsString());
    }
  }

  // Try removing each requirement in turn.
  for (unsigned victimIndex : indices(requirements)) {
    PrettyStackTraceGenericSignature debugStack("verifying", sig, victimIndex);

    // Add the requirements *except* the victim.
    SmallVector<Requirement, 2> newRequirements;
    for (unsigned i : indices(requirements)) {
      if (i != victimIndex)
        newRequirements.push_back(stripBoundDependentMemberTypes(requirements[i]));
    }

    auto newSigWithError = evaluateOrDefault(
        context.evaluator,
        AbstractGenericSignatureRequest{
          nullptr,
          genericParams,
          newRequirements,
          /*allowInverses=*/false},
        GenericSignatureWithError());

    // If there were any errors, we formed an invalid signature, so
    // just continue.
    if (newSigWithError.getInt())
      continue;

    auto newSig = newSigWithError.getPointer();

    // If the new signature once again contains the removed requirement, it's
    // not redundant.
    if (newSig->isEqual(sig))
      continue;

    // If the removed requirement is satisfied by the new generic signature,
    // it is redundant. Complain.
    auto satisfied = [&](Requirement victim) {
      if (!newSig->isValidTypeParameter(victim.getFirstType()))
        return false;

      switch (victim.getKind()) {
      case RequirementKind::SameShape:
        return (newSig->isValidTypeParameter(victim.getSecondType()) &&
                newSig->haveSameShape(victim.getFirstType(),
                                      victim.getSecondType()));
      case RequirementKind::Conformance:
        return newSig->requiresProtocol(victim.getFirstType(),
                                        victim.getProtocolDecl());
      case RequirementKind::Superclass: {
        auto superclass = newSig->getSuperclassBound(victim.getFirstType());
        return (superclass && superclass->isEqual(victim.getSecondType()));
      }
      case RequirementKind::SameType:
        if (!victim.getSecondType().findIf([&](Type t) -> bool {
            return (!t->isTypeParameter() ||
                    newSig->isValidTypeParameter(t));
          })) {
          return false;
        }
        return newSig.getReducedType(victim.getFirstType())
          ->isEqual(newSig.getReducedType(victim.getSecondType()));
      case RequirementKind::Layout: {
        auto layout = newSig->getLayoutConstraint(victim.getFirstType());
        return (layout && layout == victim.getLayoutConstraint());
      }
      }
    };

    if (satisfied(requirements[victimIndex])) {
      SmallString<32> reqString;
      {
        llvm::raw_svector_ostream out(reqString);
        requirements[victimIndex].print(out, PrintOptions());
      }
      context.Diags.diagnose(SourceLoc(), diag::generic_signature_not_minimal,
                             reqString, sig->getAsString());
    }
  }
}

void swift::validateGenericSignaturesInModule(ModuleDecl *module) {
  LoadedFile *loadedFile = nullptr;
  for (auto fileUnit : module->getFiles()) {
    loadedFile = dyn_cast<LoadedFile>(fileUnit);
    if (loadedFile) break;
  }

  if (!loadedFile) return;

  // Check all of the (canonical) generic signatures.
  SmallVector<GenericSignature, 8> allGenericSignatures;
  SmallPtrSet<CanGenericSignature, 4> knownGenericSignatures;
  (void)loadedFile->getAllGenericSignatures(allGenericSignatures);
  ASTContext &context = module->getASTContext();
  for (auto genericSig : allGenericSignatures) {
    // Check whether this is the first time we've checked this (canonical)
    // signature.
    auto canGenericSig = genericSig.getCanonicalSignature();
    if (!knownGenericSignatures.insert(canGenericSig).second) continue;

    validateGenericSignature(context, canGenericSig);
  }
}

GenericSignature
swift::buildGenericSignature(ASTContext &ctx,
                             GenericSignature baseSignature,
                             SmallVector<GenericTypeParamType *, 2> addedParameters,
                             SmallVector<Requirement, 2> addedRequirements,
                             bool allowInverses) {
  return evaluateOrDefault(
      ctx.evaluator,
      AbstractGenericSignatureRequest{
        baseSignature.getPointer(),
        addedParameters,
        addedRequirements,
        allowInverses},
      GenericSignatureWithError()).getPointer();
}

GenericSignature GenericSignature::withoutMarkerProtocols() const {
  auto requirements = getRequirements();
  SmallVector<Requirement, 4> reducedRequirements;

  // Drop all conformance requirements to marker protocols (if any).
  llvm::copy_if(requirements, std::back_inserter(reducedRequirements),
                [](const Requirement &requirement) {
                  if (requirement.getKind() == RequirementKind::Conformance) {
                    auto *protocol = requirement.getProtocolDecl();
                    return !protocol->isMarkerProtocol();
                  }
                  return true;
                });

  // If nothing changed, let's return this signature back.
  if (requirements.size() == reducedRequirements.size())
    return *this;

  return GenericSignature::get(getGenericParams(), reducedRequirements);
}

void GenericSignatureImpl::getRequirementsWithInverses(
    SmallVector<Requirement, 2> &reqs,
    SmallVector<InverseRequirement, 2> &inverses) const {
  auto &ctx = getASTContext();

  // Record the absence of conformances to invertible protocols.
  for (auto gp : getGenericParams()) {
    // Any generic parameter with a superclass bound or concrete type does not
    // have an inverse.
    if (getSuperclassBound(gp) || getConcreteType(gp))
      continue;

    // Variable generics never have inverses (or the positive thereof).
    if (gp->isValue())
      continue;

    for (auto ip : InvertibleProtocolSet::allKnown()) {
      auto *proto = ctx.getProtocol(getKnownProtocolKind(ip));

      // If we can derive a conformance to this protocol, then don't add an
      // inverse.
      if (requiresProtocol(gp, proto))
        continue;

      // Nothing implies a conformance to this protocol, so record the inverse.
      inverses.push_back({gp, proto, SourceLoc()});
    }
  }

  // Filter out explicit conformances to invertible protocols.
  for (auto req : getRequirements()) {
    if (req.isInvertibleProtocolRequirement()) {
      continue;
    }

    reqs.push_back(req);
  }
}

/// If we we can't build a requirement signature because of a request cycle or
/// failure in Knuth-Bendix completion, we give the protocol a requirement
/// signature that still has inherited protocol requirements on Self, and also
/// conformances to Copyable and Escapable for all associated types. Otherwise,
/// we'll see invariant violations from the inheritance clause mismatch, as
/// well as spurious downstream diagnostics concerning move-only types.
RequirementSignature RequirementSignature::getPlaceholderRequirementSignature(
    const ProtocolDecl *proto, GenericSignatureErrors errors) {
  auto &ctx = proto->getASTContext();

  SmallVector<ProtocolDecl *, 2> inheritedProtos;
  for (auto *inheritedProto : proto->getInheritedProtocols()) {
    inheritedProtos.push_back(inheritedProto);
  }

  for (auto ip : InvertibleProtocolSet::allKnown()) {
    auto *otherProto = ctx.getProtocol(getKnownProtocolKind(ip));
    inheritedProtos.push_back(otherProto);
  }

  ProtocolType::canonicalizeProtocols(inheritedProtos);

  SmallVector<Requirement, 2> requirements;

  for (auto *inheritedProto : inheritedProtos) {
    requirements.emplace_back(RequirementKind::Conformance,
                              proto->getSelfInterfaceType(),
                              inheritedProto->getDeclaredInterfaceType());
  }

  for (auto *assocTypeDecl : proto->getAssociatedTypeMembers()) {
    for (auto ip : InvertibleProtocolSet::allKnown()) {
      auto *otherProto = ctx.getProtocol(getKnownProtocolKind(ip));
      requirements.emplace_back(RequirementKind::Conformance,
                                assocTypeDecl->getDeclaredInterfaceType(),
                                otherProto->getDeclaredInterfaceType());
    }
  }

  // Maintain invariants.
  llvm::array_pod_sort(requirements.begin(), requirements.end(),
                       [](const Requirement *lhs, const Requirement *rhs) -> int {
                         return lhs->compare(*rhs);
                       });

  return RequirementSignature(ctx.AllocateCopy(requirements),
                              ArrayRef<ProtocolTypeAlias>());
}

void RequirementSignature::getRequirementsWithInverses(
    ProtocolDecl *owner,
    SmallVector<Requirement, 2> &reqs,
    SmallVector<InverseRequirement, 2> &inverses) const {
  auto &ctx = owner->getASTContext();
  auto sig = owner->getGenericSignature();

  llvm::SmallDenseSet<CanType, 2> assocTypes;

  auto visit = [&](Type interfaceType) {
    assocTypes.insert(interfaceType->getCanonicalType());

    // Any associated type declaration with a superclass bound or concrete type
    // does not have an inverse.
    if (sig->getSuperclassBound(interfaceType) ||
        sig->getConcreteType(interfaceType))
      return;

    for (auto ip : InvertibleProtocolSet::allKnown()) {
      auto *proto = ctx.getProtocol(getKnownProtocolKind(ip));

      // If we can derive a conformance to this protocol, then don't add an
      // inverse.
      if (sig->requiresProtocol(interfaceType, proto))
        continue;

      // Nothing implies a conformance to this protocol, so record the inverse.
      inverses.push_back({interfaceType, proto, SourceLoc()});
    }
  };

  visit(owner->getSelfInterfaceType());

  // Record the absence of conformances to invertible protocols.
  for (auto assocType : owner->getAssociatedTypeMembers()) {
    visit(assocType->getDeclaredInterfaceType());
  }

  // Filter out explicit conformances to invertible protocols.
  for (auto req : getRequirements()) {
    if (req.getKind() == RequirementKind::Conformance &&
        assocTypes.count(req.getFirstType()->getCanonicalType()) &&
        req.getProtocolDecl()->getInvertibleProtocolKind()) {
      continue;
    }

    reqs.push_back(req);
  }
}
