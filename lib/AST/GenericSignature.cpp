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

#include "GenericSignatureBuilderImpl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/RequirementMachine.h"
#include "swift/AST/Types.h"
#include "swift/Basic/STLExtras.h"
#include <functional>

using namespace swift;

void ConformanceAccessPath::print(raw_ostream &out) const {
  llvm::interleave(
      begin(), end(),
      [&](const Entry &entry) {
        entry.first.print(out);
        out << ": " << entry.second->getName();
      },
      [&] { out << " -> "; });
}

void ConformanceAccessPath::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

GenericSignatureImpl::GenericSignatureImpl(
    TypeArrayView<GenericTypeParamType> params,
    ArrayRef<Requirement> requirements, bool isKnownCanonical)
    : NumGenericParams(params.size()), NumRequirements(requirements.size()),
      CanonicalSignatureOrASTContext() {
  std::uninitialized_copy(params.begin(), params.end(),
                          getTrailingObjects<Type>());
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

TypeArrayView<GenericTypeParamType>
GenericSignatureImpl::getInnermostGenericParams() const {
  const auto params = getGenericParams();

  const unsigned maxDepth = params.back()->getDepth();
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

void GenericSignatureImpl::forEachParam(
    llvm::function_ref<void(GenericTypeParamType *, bool)> callback) const {
  // Figure out which generic parameters are concrete or same-typed to another
  // type parameter.
  auto genericParams = getGenericParams();
  auto genericParamsAreCanonical =
    SmallVector<bool, 4>(genericParams.size(), true);

  for (auto req : getRequirements()) {
    if (req.getKind() != RequirementKind::SameType) continue;

    GenericTypeParamType *gp;
    if (auto secondGP = req.getSecondType()->getAs<GenericTypeParamType>()) {
      // If two generic parameters are same-typed, then the right-hand one
      // is non-canonical.
      assert(req.getFirstType()->is<GenericTypeParamType>());
      gp = secondGP;
    } else {
      // Otherwise, the right-hand side is an associated type or concrete type,
      // and the left-hand one is non-canonical.
      gp = req.getFirstType()->getAs<GenericTypeParamType>();
      if (!gp) continue;

      // If an associated type is same-typed, it doesn't constrain the generic
      // parameter itself. That is, if T == U.Foo, then T is canonical, whereas
      // U.Foo is not.
      if (req.getSecondType()->isTypeParameter()) continue;
    }

    unsigned index = GenericParamKey(gp).findIndexIn(genericParams);
    genericParamsAreCanonical[index] = false;
  }

  // Call the callback with each parameter and the result of the above analysis.
  for (auto index : indices(genericParams))
    callback(genericParams[index], genericParamsAreCanonical[index]);
}

bool GenericSignatureImpl::areAllParamsConcrete() const {
  unsigned numConcreteGenericParams = 0;
  for (const auto &req : getRequirements()) {
    if (req.getKind() != RequirementKind::SameType) continue;
    if (!req.getFirstType()->is<GenericTypeParamType>()) continue;
    if (req.getSecondType()->isTypeParameter()) continue;

    ++numConcreteGenericParams;
  }

  return numConcreteGenericParams == getGenericParams().size();
}

ASTContext &GenericSignature::getASTContext(
                                    TypeArrayView<GenericTypeParamType> params,
                                    ArrayRef<swift::Requirement> requirements) {
  // The params and requirements cannot both be empty.
  if (!params.empty())
    return params.front()->getASTContext();
  else
    return requirements.front().getFirstType()->getASTContext();
}

GenericSignatureBuilder *
GenericSignatureImpl::getGenericSignatureBuilder() const {
  // The generic signature builder is associated with the canonical signature.
  if (!isCanonical())
    return getCanonicalSignature()->getGenericSignatureBuilder();

  // generic signature builders are stored on the ASTContext.
  return getASTContext().getOrCreateGenericSignatureBuilder(
                                             CanGenericSignature(this));
}

RequirementMachine *
GenericSignatureImpl::getRequirementMachine() const {
  // The requirement machine is associated with the canonical signature.
  if (!isCanonical())
    return getCanonicalSignature()->getRequirementMachine();

  // Requirement machines are stored on the ASTContext.
  return getASTContext().getOrCreateRequirementMachine(
                                             CanGenericSignature(this));
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
CanGenericSignature::getCanonical(TypeArrayView<GenericTypeParamType> params,
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

GenericEnvironment *GenericSignatureImpl::getGenericEnvironment() const {
  if (GenericEnv == nullptr) {
    auto *builder = getGenericSignatureBuilder();
    const auto impl = const_cast<GenericSignatureImpl *>(this);
    impl->GenericEnv = GenericEnvironment::getIncomplete(this, builder);
  }

  return GenericEnv;
}

ASTContext &GenericSignatureImpl::getASTContext() const {
  // Canonical signatures store the ASTContext directly.
  if (auto ctx = CanonicalSignatureOrASTContext.dyn_cast<ASTContext *>())
    return *ctx;

  // For everything else, just get it from the generic parameter.
  return GenericSignature::getASTContext(getGenericParams(), getRequirements());
}

ProtocolConformanceRef
GenericSignatureImpl::lookupConformance(CanType type,
                                        ProtocolDecl *proto) const {
  // FIXME: Actually implement this properly.
  auto *M = proto->getParentModule();

  if (type->isTypeParameter())
    return ProtocolConformanceRef(proto);

  return M->lookupConformance(type, proto);
}

bool GenericSignatureImpl::requiresClass(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have superclass requirements");

  auto computeViaGSB = [&]() {
    auto &builder = *getGenericSignatureBuilder();
    auto equivClass =
      builder.resolveEquivalenceClass(
                                    type,
                                    ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return false;

    // If this type was mapped to a concrete type, then there is no
    // requirement.
    if (equivClass->concreteType) return false;

    // If there is a layout constraint, it might be a class.
    if (equivClass->layout && equivClass->layout->isClass()) return true;

    return false;
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->requiresClass(type);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    bool rqmResult = computeViaRQM();

#ifndef NDEBUG
    bool gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::requiresClass() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

/// Determine the superclass bound on the given dependent type.
Type GenericSignatureImpl::getSuperclassBound(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have superclass requirements");

  auto &builder = *getGenericSignatureBuilder();
  auto equivClass =
  builder.resolveEquivalenceClass(
                                type,
                                ArchetypeResolutionKind::CompleteWellFormed);
  if (!equivClass) return nullptr;

  // If this type was mapped to a concrete type, then there is no
  // requirement.
  if (equivClass->concreteType) return nullptr;

  // Retrieve the superclass bound.
  return equivClass->superclass;
}

/// Determine the set of protocols to which the given type parameter is
/// required to conform.
GenericSignature::RequiredProtocols
GenericSignatureImpl::getRequiredProtocols(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  auto computeViaGSB = [&]() -> GenericSignature::RequiredProtocols {
    auto &builder = *getGenericSignatureBuilder();
    auto equivClass =
      builder.resolveEquivalenceClass(
                                    type,
                                    ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return { };

    // If this type parameter was mapped to a concrete type, then there
    // are no requirements.
    if (equivClass->concreteType) return { };

    // Retrieve the protocols to which this type conforms.
    GenericSignature::RequiredProtocols result;
    for (const auto &conforms : equivClass->conformsTo)
      result.push_back(conforms.first);

    // Canonicalize the resulting set of protocols.
    ProtocolType::canonicalizeProtocols(result);

    return result;
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->getRequiredProtocols(type);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    auto rqmResult = computeViaRQM();

#ifndef NDEBUG
    auto gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::getRequiredProtocols() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "GenericSignatureBuilder says: ";
      for (auto *otherProto : gsbResult)
        llvm::errs() << " " << otherProto->getName();
      llvm::errs() << "\n";
      llvm::errs() << "RequirementMachine says: ";
      for (auto *otherProto : rqmResult)
        llvm::errs() << " " << otherProto->getName();
      llvm::errs() << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

bool GenericSignatureImpl::requiresProtocol(Type type,
                                            ProtocolDecl *proto) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  auto computeViaGSB = [&]() {
    auto &builder = *getGenericSignatureBuilder();
    auto equivClass =
      builder.resolveEquivalenceClass(
                                    type,
                                    ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return false;

    // FIXME: Optionally deal with concrete conformances here
    // or have a separate method do that additionally?
    //
    // If this type parameter was mapped to a concrete type, then there
    // are no requirements.
    if (equivClass->concreteType) return false;

    // Check whether the representative conforms to this protocol.
    return equivClass->conformsTo.count(proto) > 0;
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->requiresProtocol(type, proto);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    bool rqmResult = computeViaRQM();

#ifndef NDEBUG
    bool gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::requiresProtocol() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "Protocol: "; proto->dumpRef(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

/// Determine whether the given dependent type is equal to a concrete type.
bool GenericSignatureImpl::isConcreteType(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  auto computeViaGSB = [&]() {
    auto &builder = *getGenericSignatureBuilder();
    auto equivClass =
      builder.resolveEquivalenceClass(
                                    type,
                                    ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return false;

    return bool(equivClass->concreteType);
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->isConcreteType(type);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    bool rqmResult = computeViaRQM();

#ifndef NDEBUG
    bool gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::isConcreteType() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

/// Return the concrete type that the given type parameter is constrained to,
/// or the null Type if it is not the subject of a concrete same-type
/// constraint.
Type GenericSignatureImpl::getConcreteType(Type type) const {
  assert(type->isTypeParameter() && "Expected a type parameter");

  auto &builder = *getGenericSignatureBuilder();
  auto equivClass =
    builder.resolveEquivalenceClass(
                                  type,
                                  ArchetypeResolutionKind::CompleteWellFormed);
  if (!equivClass) return Type();

  return equivClass->concreteType;
}

LayoutConstraint GenericSignatureImpl::getLayoutConstraint(Type type) const {
  assert(type->isTypeParameter() &&
         "Only type parameters can have layout constraints");

  auto computeViaGSB = [&]() {
    auto &builder = *getGenericSignatureBuilder();
    auto equivClass =
      builder.resolveEquivalenceClass(
                                    type,
                                    ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return LayoutConstraint();

    return equivClass->layout;
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->getLayoutConstraint(type);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    auto rqmResult = computeViaRQM();

#ifndef NDEBUG
    auto gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::getLayoutConstraint() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

bool GenericSignatureImpl::areSameTypeParameterInContext(Type type1,
                                                         Type type2) const {
  assert(type1->isTypeParameter());
  assert(type2->isTypeParameter());

  if (type1.getPointer() == type2.getPointer())
    return true;

  auto computeViaGSB = [&]() {
    return areSameTypeParameterInContext(type1, type2,
                                         *getGenericSignatureBuilder());
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->areSameTypeParameterInContext(type1, type2);
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    auto rqmResult = computeViaRQM();

#ifndef NDEBUG
    auto gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::areSameTypeParameterInContext() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "First dependent type: "; type1.dump(llvm::errs());
      llvm::errs() << "Second dependent type: "; type2.dump(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

bool GenericSignatureImpl::areSameTypeParameterInContext(Type type1,
                                                         Type type2,
                                                         GenericSignatureBuilder &builder) const {
  assert(type1->isTypeParameter());
  assert(type2->isTypeParameter());

  if (type1.getPointer() == type2.getPointer())
    return true;

  auto equivClass1 =
    builder.resolveEquivalenceClass(
                             type1,
                             ArchetypeResolutionKind::CompleteWellFormed);
  assert(equivClass1 && "not a valid dependent type of this signature?");

  auto equivClass2 =
    builder.resolveEquivalenceClass(
                             type2,
                             ArchetypeResolutionKind::CompleteWellFormed);
  assert(equivClass2 && "not a valid dependent type of this signature?");

  return equivClass1 == equivClass2;
}

bool GenericSignatureImpl::isRequirementSatisfied(
    Requirement requirement) const {
  if (requirement.getFirstType()->hasTypeParameter()) {
    auto *genericEnv = getGenericEnvironment();

    auto substituted = requirement.subst(
        [&](SubstitutableType *type) -> Type {
          if (auto *paramType = type->getAs<GenericTypeParamType>())
            return genericEnv->mapTypeIntoContext(paramType);

          return type;
        },
        LookUpConformanceInSignature(this));

    if (!substituted)
      return false;

    requirement = *substituted;
  }

  // FIXME: Need to check conditional requirements here.
  ArrayRef<Requirement> conditionalRequirements;

  return requirement.isSatisfied(conditionalRequirements);
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

bool GenericSignatureImpl::isCanonicalTypeInContext(Type type) const {
  // If the type isn't independently canonical, it's certainly not canonical
  // in this context.
  if (!type->isCanonical())
    return false;

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return true;

  auto &builder = *getGenericSignatureBuilder();
  return isCanonicalTypeInContext(type, builder);
}

bool GenericSignatureImpl::isCanonicalTypeInContext(
    Type type, GenericSignatureBuilder &builder) const {
  // If the type isn't independently canonical, it's certainly not canonical
  // in this context.
  if (!type->isCanonical())
    return false;

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return true;

  // Look for non-canonical type parameters.
  return !type.findIf([&](Type component) -> bool {
    if (!component->isTypeParameter()) return false;

    auto equivClass =
      builder.resolveEquivalenceClass(
                               Type(component),
                               ArchetypeResolutionKind::CompleteWellFormed);
    if (!equivClass) return false;

    return (equivClass->concreteType ||
            !component->isEqual(equivClass->getAnchor(builder,
                                                      getGenericParams())));
  });
}

CanType GenericSignatureImpl::getCanonicalTypeInContext(Type type) const {
  type = type->getCanonicalType();

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return CanType(type);

  auto computeViaGSB = [&]() {
    auto &builder = *getGenericSignatureBuilder();
    return builder.getCanonicalTypeInContext(type, { })->getCanonicalType();
  };

  auto computeViaRQM = [&]() {
    auto *machine = getRequirementMachine();
    return machine->getCanonicalTypeInContext(type, { })->getCanonicalType();
  };

  auto &ctx = getASTContext();
  if (ctx.LangOpts.EnableRequirementMachine) {
    auto rqmResult = computeViaRQM();

#ifndef NDEBUG
    auto gsbResult = computeViaGSB();

    if (gsbResult != rqmResult) {
      llvm::errs() << "RequirementMachine::getCanonicalTypeInContext() is broken\n";
      llvm::errs() << "Generic signature: " << GenericSignature(this) << "\n";
      llvm::errs() << "Dependent type: "; type.dump(llvm::errs());
      llvm::errs() << "GenericSignatureBuilder says: " << gsbResult << "\n";
      gsbResult.dump(llvm::errs());
      llvm::errs() << "RequirementMachine says: " << rqmResult << "\n";
      rqmResult.dump(llvm::errs());
      llvm::errs() << "\n";
      getRequirementMachine()->dump(llvm::errs());
      abort();
    }
#endif

    return rqmResult;
  } else {
    return computeViaGSB();
  }
}

ArrayRef<CanTypeWrapper<GenericTypeParamType>>
CanGenericSignature::getGenericParams() const{
  auto params = getPointer()->getGenericParams().getOriginalArray();
  auto base = static_cast<const CanTypeWrapper<GenericTypeParamType>*>(
                                                              params.data());
  return {base, params.size()};
}

ConformanceAccessPath
GenericSignatureImpl::getConformanceAccessPath(Type type,
                                               ProtocolDecl *protocol) const {
  return getGenericSignatureBuilder()->getConformanceAccessPath(
      type, protocol, this);
}

unsigned GenericParamKey::findIndexIn(
                      TypeArrayView<GenericTypeParamType> genericParams) const {
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
                                return Type(cast<GenericTypeParamType>(t));
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

  return type.transform([this](Type Ty) -> Type {
    if (auto GP = dyn_cast<GenericTypeParamType>(Ty.getPointer())) {
      return Type(getSugaredType(GP));
    }
    return Ty;
  });
}

unsigned GenericSignatureImpl::getGenericParamOrdinal(
    GenericTypeParamType *param) const {
  return GenericParamKey(param).findIndexIn(getGenericParams());
}

bool GenericSignatureImpl::hasTypeVariable() const {
  return GenericSignature::hasTypeVariable(getRequirements());
}

bool GenericSignature::hasTypeVariable(ArrayRef<Requirement> requirements) {
  for (const auto &req : requirements) {
    if (req.getFirstType()->hasTypeVariable())
      return true;

    switch (req.getKind()) {
    case RequirementKind::Layout:
      break;

    case RequirementKind::Conformance:
    case RequirementKind::SameType:
    case RequirementKind::Superclass:
      if (req.getSecondType()->hasTypeVariable())
        return true;
      break;
    }
  }

  return false;
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &id) const {
  return GenericSignature::Profile(id, getPointer()->getGenericParams(),
                                   getPointer()->getRequirements());
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &ID,
                               TypeArrayView<GenericTypeParamType> genericParams,
                               ArrayRef<Requirement> requirements) {
  return GenericSignatureImpl::Profile(ID, genericParams, requirements);
}

void swift::simple_display(raw_ostream &out, GenericSignature sig) {
  if (sig)
    sig->print(out);
  else
    out << "NULL";
}

bool Requirement::isCanonical() const {
  if (getFirstType() && !getFirstType()->isCanonical())
    return false;

  switch (getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::SameType:
  case RequirementKind::Superclass:
    if (getSecondType() && !getSecondType()->isCanonical())
      return false;
    break;

  case RequirementKind::Layout:
    break;
  }

  return true;
}

/// Get the canonical form of this requirement.
Requirement Requirement::getCanonical() const {
  Type firstType = getFirstType();
  if (firstType)
    firstType = firstType->getCanonicalType();

  switch (getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::SameType:
  case RequirementKind::Superclass: {
    Type secondType = getSecondType();
    if (secondType)
      secondType = secondType->getCanonicalType();
    return Requirement(getKind(), firstType, secondType);
  }

  case RequirementKind::Layout:
    return Requirement(getKind(), firstType, getLayoutConstraint());
  }
  llvm_unreachable("Unhandled RequirementKind in switch");
}

ProtocolDecl *Requirement::getProtocolDecl() const {
  assert(getKind() == RequirementKind::Conformance);
  return getSecondType()->castTo<ProtocolType>()->getDecl();
}

bool
Requirement::isSatisfied(ArrayRef<Requirement> &conditionalRequirements) const {
  switch (getKind()) {
  case RequirementKind::Conformance: {
    auto *proto = getProtocolDecl();
    auto *module = proto->getParentModule();
    auto conformance = module->lookupConformance(getFirstType(), proto);
    if (!conformance)
      return false;

    conditionalRequirements = conformance.getConditionalRequirements();
    return true;
  }

  case RequirementKind::Layout: {
    if (auto *archetypeType = getFirstType()->getAs<ArchetypeType>()) {
      auto layout = archetypeType->getLayoutConstraint();
      return (layout && layout.merge(getLayoutConstraint()));
    }

    if (getLayoutConstraint()->isClass())
      return getFirstType()->satisfiesClassConstraint();

    // TODO: Statically check other layout constraints, once they can
    // be spelled in Swift.
    return true;
  }

  case RequirementKind::Superclass:
    return getSecondType()->isExactSuperclassOf(getFirstType());

  case RequirementKind::SameType:
    return getFirstType()->isEqual(getSecondType());
  }

  llvm_unreachable("Bad requirement kind");
}

bool Requirement::canBeSatisfied() const {
  switch (getKind()) {
  case RequirementKind::Conformance:
    return getFirstType()->is<ArchetypeType>();

  case RequirementKind::Layout: {
    if (auto *archetypeType = getFirstType()->getAs<ArchetypeType>()) {
      auto layout = archetypeType->getLayoutConstraint();
      return (!layout || layout.merge(getLayoutConstraint()));
    }

    return false;
  }

  case RequirementKind::Superclass:
    return (getFirstType()->isBindableTo(getSecondType()) ||
            getSecondType()->isBindableTo(getFirstType()));

  case RequirementKind::SameType:
    return (getFirstType()->isBindableTo(getSecondType()) ||
            getSecondType()->isBindableTo(getFirstType()));
  }

  llvm_unreachable("Bad requirement kind");
}