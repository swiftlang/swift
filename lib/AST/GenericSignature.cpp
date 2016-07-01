//===--- GenericSignature.cpp - Generic Signature AST ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the GenericSignature class.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
using namespace swift;

GenericSignature::GenericSignature(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements,
                                   bool isKnownCanonical)
  : NumGenericParams(params.size()), NumRequirements(requirements.size()),
    CanonicalSignatureOrASTContext()
{
  auto paramsBuffer = getGenericParamsBuffer();
  for (unsigned i = 0; i < NumGenericParams; ++i) {
    paramsBuffer[i] = params[i];
  }

  auto reqtsBuffer = getRequirementsBuffer();
  for (unsigned i = 0; i < NumRequirements; ++i) {
    reqtsBuffer[i] = requirements[i];
  }

  if (isKnownCanonical)
    CanonicalSignatureOrASTContext = &getASTContext(params, requirements);
}

ArrayRef<GenericTypeParamType *> 
GenericSignature::getInnermostGenericParams() const {
  auto params = getGenericParams();

  // Find the point at which the depth changes.
  unsigned depth = params.back()->getDepth();
  for (unsigned n = params.size(); n > 0; --n) {
    if (params[n-1]->getDepth() != depth) {
      return params.slice(n);
    }
  }

  // All parameters are at the same depth.
  return params;
}

ASTContext &GenericSignature::getASTContext(
                                ArrayRef<swift::GenericTypeParamType *> params,
                                ArrayRef<swift::Requirement> requirements) {
  // The params and requirements cannot both be empty.
  if (!params.empty())
    return params.front()->getASTContext();
  else
    return requirements.front().getFirstType()->getASTContext();
}

ArchetypeBuilder *GenericSignature::getArchetypeBuilder(ModuleDecl &mod) {
  // The archetype builder is associated with the canonical signature.
  if (!isCanonical())
    return getCanonicalSignature()->getArchetypeBuilder(mod);

  // Archetype builders are stored on the ASTContext.
  return getASTContext().getOrCreateArchetypeBuilder(CanGenericSignature(this),
                                                     &mod);
}

bool GenericSignature::isCanonical() const {
  if (CanonicalSignatureOrASTContext.is<ASTContext*>()) return true;

  return getCanonicalSignature() == this;
}

CanGenericSignature GenericSignature::getCanonical(
                                        ArrayRef<GenericTypeParamType *> params,
                                        ArrayRef<Requirement> requirements) {
  // Canonicalize the parameters and requirements.
  SmallVector<GenericTypeParamType*, 8> canonicalParams;
  canonicalParams.reserve(params.size());
  for (auto param : params) {
    canonicalParams.push_back(cast<GenericTypeParamType>(param->getCanonicalType()));
  }

  SmallVector<Requirement, 8> canonicalRequirements;
  canonicalRequirements.reserve(requirements.size());
  for (auto &reqt : requirements) {
    canonicalRequirements.push_back(Requirement(reqt.getKind(),
                              reqt.getFirstType()->getCanonicalType(),
                              reqt.getSecondType().getCanonicalTypeOrNull()));
  }
  auto canSig = get(canonicalParams, canonicalRequirements,
                    /*isKnownCanonical=*/true);
  return CanGenericSignature(canSig);
}

CanGenericSignature
GenericSignature::getCanonicalSignature() const {
  // If we haven't computed the canonical signature yet, do so now.
  if (CanonicalSignatureOrASTContext.isNull()) {
    // Compute the canonical signature.
    CanGenericSignature canSig = getCanonical(getGenericParams(),
                                              getRequirements());

    // Record either the canonical signature or an indication that
    // this is the canonical signature.
    if (canSig != this)
      CanonicalSignatureOrASTContext = canSig;
    else
      CanonicalSignatureOrASTContext = &getGenericParams()[0]->getASTContext();

    // Return the canonical signature.
    return canSig;
  }

  // A stored ASTContext indicates that this is the canonical
  // signature.
  if (CanonicalSignatureOrASTContext.is<ASTContext*>())
    // TODO: CanGenericSignature should be const-correct.
    return CanGenericSignature(const_cast<GenericSignature*>(this));
  
  // Otherwise, return the stored canonical signature.
  return CanGenericSignature(
           CanonicalSignatureOrASTContext.get<GenericSignature*>());
}

/// Canonical ordering for dependent types in generic signatures.
static int compareDependentTypes(const CanType *pa, const CanType *pb) {
  auto a = *pa, b = *pb;
  
  // Fast-path check for equality.
  if (a == b)
    return 0;

  // Ordering is as follows:
  // - Generic params
  if (auto gpa = dyn_cast<GenericTypeParamType>(a)) {
    if (auto gpb = dyn_cast<GenericTypeParamType>(b)) {
      // - by depth, so t_0_n < t_1_m
      if (int compareDepth = gpa->getDepth() - gpb->getDepth())
        return compareDepth;
      // - by index, so t_n_0 < t_n_1
      return gpa->getIndex() - gpb->getIndex();
    }
    return -1;
  }
  
  // - Dependent members
  if (auto dma = dyn_cast<DependentMemberType>(a)) {
    if (isa<GenericTypeParamType>(b))
      return +1;
    if (auto dmb = dyn_cast<DependentMemberType>(b)) {
      // - by base, so t_0_n.`P.T` < t_1_m.`P.T`
      auto abase = dma.getBase();
      auto bbase = dmb.getBase();
      if (int compareBases = compareDependentTypes(&abase, &bbase))
        return compareBases;
      
      // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
      auto protoa = dma->getAssocType()->getProtocol();
      auto protob = dmb->getAssocType()->getProtocol();
      if (int compareProtocols
            = ProtocolType::compareProtocols(&protoa, &protob))
        return compareProtocols;
      
      // - by name, so t_n_m.`P.T` < t_n_m.`P.U`
      return dma->getAssocType()->getName().str().compare(
                                          dmb->getAssocType()->getName().str());
    }
    return -1;
  }
  
  // - Other types.
  //
  // There should only ever be one of these in a set of constraints related to
  // a dependent type, so the ordering among other types does not matter.
  if (isa<GenericTypeParamType>(b) || isa<DependentMemberType>(b))
    return +1;
  return 0;
}

CanGenericSignature
GenericSignature::getCanonicalManglingSignature(ModuleDecl &M) const {
  // Start from the elementwise-canonical signature.
  auto canonical = getCanonicalSignature();
  auto &Context = canonical->getASTContext();
  
  // See if we cached the mangling signature.
  auto cached = Context.ManglingSignatures.find({canonical, &M});
  if (cached != Context.ManglingSignatures.end()) {
    return cached->second;
  }
  
  // Otherwise, we need to compute it.
  // Dump the generic signature into an ArchetypeBuilder that will figure out
  // the minimal set of requirements.
  std::unique_ptr<ArchetypeBuilder> builder(new ArchetypeBuilder(M, 
                                                                 Context.Diags));
  
  builder->addGenericSignature(canonical, /*adoptArchetypes*/ false,
                               /*treatRequirementsAsExplicit*/ true);
  
  // Sort out the requirements.
  struct DependentConstraints {
    CanType baseClass;
    SmallVector<CanType, 2> protocols;
  };
  
  SmallVector<CanType, 2> depTypes;
  llvm::DenseMap<CanType, DependentConstraints> constraints;
  llvm::DenseMap<CanType, SmallVector<CanType, 2>> sameTypes;
  
  builder->enumerateRequirements([&](RequirementKind kind,
          ArchetypeBuilder::PotentialArchetype *archetype,
          llvm::PointerUnion<Type, ArchetypeBuilder::PotentialArchetype *> type,
          RequirementSource source) {
    CanType depTy
      = archetype->getDependentType(*builder, false)->getCanonicalType();
    
    // Filter out redundant requirements.
    switch (source.getKind()) {
    case RequirementSource::Explicit:
      // The requirement was explicit and required, keep it.
      break;
      
    case RequirementSource::Protocol:
      // Keep witness markers.
      if (kind == RequirementKind::WitnessMarker)
        break;
      return;
    
    case RequirementSource::Redundant:
    case RequirementSource::Inferred:
      // The requirement was inferred or redundant, drop it.
      return;
      
    case RequirementSource::OuterScope:
      llvm_unreachable("shouldn't have an outer scope!");
    }
    
    switch (kind) {
    case RequirementKind::WitnessMarker: {
      // Introduce the dependent type into the constraint set, to ensure we
      // have a record for every dependent type.
      depTypes.push_back(depTy);
      return;
    }

    case RequirementKind::Superclass: {
      assert(std::find(depTypes.begin(), depTypes.end(),
                       depTy) != depTypes.end()
             && "didn't see witness marker first?");
      // Organize conformance constraints, sifting out the base class
      // requirement.
      auto &depConstraints = constraints[depTy];

      auto constraintType = type.get<Type>()->getCanonicalType();
      assert(depConstraints.baseClass.isNull()
              && "multiple base class constraints?!");
      depConstraints.baseClass = constraintType;
      return;
    }
      
    case RequirementKind::Conformance: {
      assert(std::find(depTypes.begin(), depTypes.end(),
                       depTy) != depTypes.end()
             && "didn't see witness marker first?");
      // Organize conformance constraints, sifting out the base class
      // requirement.
      auto &depConstraints = constraints[depTy];
      
      auto constraintType = type.get<Type>()->getCanonicalType();
      assert(constraintType->isExistentialType());
      depConstraints.protocols.push_back(constraintType);
      return;
    }
    
    case RequirementKind::SameType:
      // Collect the same-type constraints by their representative.
      CanType repTy;
      if (auto concreteTy = type.dyn_cast<Type>()) {
        // Maybe we were equated to a concrete type...
        repTy = concreteTy->getCanonicalType();
      } else {
        // ...or to a representative dependent type that was in turn equated
        // to a concrete type.
        auto representative
          = type.get<ArchetypeBuilder::PotentialArchetype *>();
        
        if (representative->isConcreteType())
          repTy = representative->getConcreteType()->getCanonicalType();
        else
          repTy = representative->getDependentType(*builder, false)
            ->getCanonicalType();
      }
      
      sameTypes[repTy].push_back(depTy);
      return;
    }
  });
  
  // Order the dependent types canonically.
  llvm::array_pod_sort(depTypes.begin(), depTypes.end(), compareDependentTypes);
  
  // Build a new set of minimized requirements.
  // Emit the conformance constraints.
  SmallVector<Requirement, 4> minimalRequirements;
  for (auto depTy : depTypes) {
    minimalRequirements.push_back(Requirement(RequirementKind::WitnessMarker,
                                              depTy, Type()));
    
    auto foundConstraints = constraints.find(depTy);
    if (foundConstraints != constraints.end()) {
      const auto &depConstraints = foundConstraints->second;
      
      if (depConstraints.baseClass)
        minimalRequirements.push_back(Requirement(RequirementKind::Superclass,
                                                  depTy,
                                                  depConstraints.baseClass));
      
      for (auto protocol : depConstraints.protocols)
        minimalRequirements.push_back(Requirement(RequirementKind::Conformance,
                                                  depTy, protocol));
    }
  }
  
  // Collect the same type constraints.
  unsigned sameTypeBegin = minimalRequirements.size();
  
  for (auto &group : sameTypes) {
    // Sort the types in the set.
    auto types = std::move(group.second);
    types.push_back(group.first);
    llvm::array_pod_sort(types.begin(), types.end(), compareDependentTypes);

    // Form constraints with the greater type on the right (which will be the
    // concrete type, if one).
    auto rhsType = types.pop_back_val();
    for (auto lhsType : types)
      minimalRequirements.push_back(Requirement(RequirementKind::SameType,
                                                lhsType, rhsType));
  }
  
  // Sort the same-types by LHS, then by RHS.
  std::sort(minimalRequirements.begin() + sameTypeBegin, minimalRequirements.end(),
    [](const Requirement &a, const Requirement &b) -> bool {
      assert(a.getKind() == b.getKind()
             && a.getKind() == RequirementKind::SameType
             && "not same type constraints");
      CanType aLHS(a.getFirstType()), bLHS(b.getFirstType());
      if (int compareLHS = compareDependentTypes(&aLHS, &bLHS))
        return compareLHS < 0;
      CanType aRHS(a.getSecondType()), bRHS(b.getSecondType());
      return compareDependentTypes(&aRHS, &bRHS);
    });
  
  // Build the minimized signature.
  auto manglingSig = GenericSignature::get(canonical->getGenericParams(),
                                           minimalRequirements,
                                           /*isKnownCanonical=*/true);
  
  CanGenericSignature canSig(manglingSig);
  
  // Cache the result.
  Context.ManglingSignatures.insert({{canonical, &M}, canSig});
  Context.setArchetypeBuilder(canSig, &M, std::move(builder));

  return canSig;
}

ASTContext &GenericSignature::getASTContext() const {
  // Canonical signatures store the ASTContext directly.
  if (auto ctx = CanonicalSignatureOrASTContext.dyn_cast<ASTContext *>())
    return *ctx;

  // For everything else, just get it from the generic parameter.
  return getASTContext(getGenericParams(), getRequirements());
}

TypeSubstitutionMap
GenericSignature::getSubstitutionMap(ArrayRef<Substitution> args) const {
  TypeSubstitutionMap subs;
  
  // An empty parameter list gives an empty map.
  if (getGenericParams().empty()) {
    assert(args.empty() && "substitutions but no generic params?!");
    return subs;
  }
  
  // Seed the type map with pre-existing substitutions.
  for (auto depTy : getAllDependentTypes()) {
    auto replacement = args.front().getReplacement();
    args = args.slice(1);
    
    if (auto subTy = depTy->getAs<SubstitutableType>()) {
      subs[subTy->getCanonicalType().getPointer()] = replacement;
    }
    else if (auto dTy = depTy->getAs<DependentMemberType>()) {
      subs[dTy->getCanonicalType().getPointer()] = replacement;
    }
  }
  
  assert(args.empty() && "did not use all substitutions?!");
  return subs;
}

bool GenericSignature::requiresClass(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return false;

  auto &builder = *getArchetypeBuilder(mod);
  auto pa = builder.resolveArchetype(type);
  if (!pa) return false;

  pa = pa->getRepresentative();

  // If this type was mapped to a concrete type, then there is no
  // requirement.
  if (pa->isConcreteType()) return false;

  // If there is a superclass bound, then obviously it must be a class.
  if (pa->getSuperclass()) return true;

  // If any of the protocols are class-bound, then it must be a class.
  for (auto proto : pa->getConformsTo()) {
    if (proto.first->requiresClass()) return true;
  }

  return false;
}

/// Determine the superclass bound on the given dependent type.
Type GenericSignature::getSuperclassBound(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return nullptr;

  auto &builder = *getArchetypeBuilder(mod);
  auto pa = builder.resolveArchetype(type);
  if (!pa) return nullptr;

  pa = pa->getRepresentative();

  // If this type was mapped to a concrete type, then there is no
  // requirement.
  if (pa->isConcreteType()) return nullptr;

  // Retrieve the superclass bound.
  return pa->getSuperclass();
}

/// Determine the set of protocols to which the given dependent type
/// must conform.
SmallVector<ProtocolDecl *, 2> GenericSignature::getConformsTo(Type type,
                                                               ModuleDecl &mod) {
  if (!type->isTypeParameter()) return { };

  auto &builder = *getArchetypeBuilder(mod);
  auto pa = builder.resolveArchetype(type);
  if (!pa) return { };

  pa = pa->getRepresentative();

  // If this type was mapped to a concrete type, then there are no
  // requirements.
  if (pa->isConcreteType()) return { };

  // Retrieve the protocols to which this type conforms.
  SmallVector<ProtocolDecl *, 2> result;
  for (auto proto : pa->getConformsTo())
    result.push_back(proto.first);

  // Canonicalize the resulting set of protocols.
  ProtocolType::canonicalizeProtocols(result);

  return result;
}

/// Determine whether the given dependent type is equal to a concrete type.
bool GenericSignature::isConcreteType(Type type, ModuleDecl &mod) {
  return bool(getConcreteType(type, mod));
}

/// Return the concrete type that the given dependent type is constrained to,
/// or the null Type if it is not the subject of a concrete same-type
/// constraint.
Type GenericSignature::getConcreteType(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return Type();

  auto &builder = *getArchetypeBuilder(mod);
  auto pa = builder.resolveArchetype(type);
  if (!pa) return Type();

  pa = pa->getRepresentative();
  if (!pa->isConcreteType()) return Type();

  return pa->getConcreteType();
}

Type GenericSignature::getRepresentative(Type type, ModuleDecl &mod) {
  assert(type->isTypeParameter());
  auto &builder = *getArchetypeBuilder(mod);
  auto pa = builder.resolveArchetype(type);
  assert(pa && "not a valid dependent type of this signature?");
  auto rep = pa->getRepresentative();
  if (rep->isConcreteType()) return rep->getConcreteType();
  if (pa == rep) {
    assert(rep->getDependentType(builder, /*allowUnresolved*/ false)
              ->getCanonicalType() == type->getCanonicalType());
    return type;
  }
  return rep->getDependentType(builder, /*allowUnresolved*/ false);
}

bool GenericSignature::areSameTypeParameterInContext(Type type1, Type type2,
                                                     ModuleDecl &mod) {
  assert(type1->isTypeParameter());
  assert(type2->isTypeParameter());

  if (type1.getPointer() == type2.getPointer())
    return true;

  auto &builder = *getArchetypeBuilder(mod);
  auto pa1 = builder.resolveArchetype(type1);
  assert(pa1 && "not a valid dependent type of this signature?");
  pa1 = pa1->getRepresentative();
  assert(!pa1->isConcreteType());

  auto pa2 = builder.resolveArchetype(type2);
  assert(pa2 && "not a valid dependent type of this signature?");
  pa2 = pa2->getRepresentative();
  assert(!pa2->isConcreteType());

  return pa1 == pa2;
}

bool GenericSignature::isCanonicalTypeInContext(Type type, ModuleDecl &mod) {
  // If the type isn't independently canonical, it's certainly not canonical
  // in this context.
  if (!type->isCanonical())
    return false;

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return true;

  auto &builder = *getArchetypeBuilder(mod);

  // Look for non-canonical type parameters.
  return !type.findIf([&](Type component) -> bool {
    if (!component->isTypeParameter()) return false;

    auto pa = builder.resolveArchetype(component);
    if (!pa) return false;

    auto rep = pa->getArchetypeAnchor();
    return (rep->isConcreteType() || pa != rep);
  });
}

CanType GenericSignature::getCanonicalTypeInContext(Type type, ModuleDecl &mod) {
  type = type->getCanonicalType();

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return CanType(type);

  auto &builder = *getArchetypeBuilder(mod);

  // Replace non-canonical type parameters.
  type = type.transform([&](Type component) -> Type {
    if (!component->isTypeParameter()) return component;

    // Resolve the potential archetype.  This can be null in nested generic
    // types, which we can't immediately canonicalize.
    auto pa = builder.resolveArchetype(component);
    if (!pa) return component;

    auto rep = pa->getArchetypeAnchor();
    if (rep->isConcreteType()) {
      return getCanonicalTypeInContext(rep->getConcreteType(), mod);
    } else {
      return rep->getDependentType(builder, /*allowUnresolved*/ false);
    }
  });

  return type->getCanonicalType();
}
