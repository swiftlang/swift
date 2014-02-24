//===--- ProtocolConformance.cpp - AST Protocol Conformance -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeWalker.h"

using namespace swift;

void *ProtocolConformance::operator new(size_t bytes, ASTContext &context,
                                        AllocationArena arena,
                                        unsigned alignment) {
  return context.Allocate(bytes, alignment, arena);

}

#define CONFORMANCE_SUBCLASS_DISPATCH(Method, Args)                        \
switch (getKind()) {                                                       \
  case ProtocolConformanceKind::Normal:                                    \
    static_assert(&ProtocolConformance::Method !=                          \
                    &NormalProtocolConformance::Method,                    \
                  "Must override NormalProtocolConformance::" #Method);    \
    return cast<NormalProtocolConformance>(this)->Method Args;             \
  case ProtocolConformanceKind::Specialized:                               \
    static_assert(&ProtocolConformance::Method !=                          \
                    &InheritedProtocolConformance::Method,                 \
                  "Must override InheritedProtocolConformance::" #Method); \
    return cast<SpecializedProtocolConformance>(this)->Method Args;        \
  case ProtocolConformanceKind::Inherited:                                 \
    static_assert(&ProtocolConformance::Method !=                          \
                    &InheritedProtocolConformance::Method,                 \
                  "Must override InheritedProtocolConformance::" #Method); \
    return cast<InheritedProtocolConformance>(this)->Method Args;          \
}

/// Get the protocol being conformed to.
ProtocolDecl *ProtocolConformance::getProtocol() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getProtocol, ())
}

DeclContext *ProtocolConformance::getDeclContext() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getDeclContext, ())
}

/// Retrieve the state of this conformance.
ProtocolConformanceState ProtocolConformance::getState() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getState, ())
}

const Substitution &
ProtocolConformance::getTypeWitness(AssociatedTypeDecl *assocType, 
                                    LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitness, (assocType, resolver))
}

ConcreteDeclRef ProtocolConformance::getWitness(ValueDecl *requirement,
                                               LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getWitness, (requirement, resolver))
}

const InheritedConformanceMap &
ProtocolConformance::getInheritedConformances() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getInheritedConformances, ())
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::usesDefaultDefinition(ValueDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
}

GenericParamList *ProtocolConformance::getGenericParams() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    // FIXME: This should be an independent property of the conformance.
    // Assuming a BoundGenericType conformance is always for the
    // DeclaredTypeInContext is unsound if we ever add constrained extensions.
    Type ty = getType();
    while (ty) {
      if (auto nt = ty->getAs<NominalType>())
        ty = nt->getParent();
      else
        break;
    }
    
    if (!ty)
      return nullptr;
    
    if (auto bgt = ty->getAs<BoundGenericType>()) {
      auto decl = bgt->getDecl();
      assert(bgt->isEqual(decl->getDeclaredTypeInContext())
             && "conformance for constrained generic type not implemented");
      return decl->getGenericParams();
    }
    return nullptr;
  }
  case ProtocolConformanceKind::Specialized:
  case ProtocolConformanceKind::Inherited:
    // FIXME: These could reasonably have open type variables.
    return nullptr;
  }
}

Type ProtocolConformance::getInterfaceType() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    // FIXME: This should be the type stored in the protocol conformance.
    // Assuming a generic conformance is always for the DeclaredTypeInContext
    // is unsound if we ever add constrained extensions.
    return getType()->getNominalOrBoundGenericNominal()
      ->getDeclaredInterfaceType();
  
  case ProtocolConformanceKind::Inherited:
    return cast<InheritedProtocolConformance>(this)->getInheritedConformance()
      ->getInterfaceType();

  case ProtocolConformanceKind::Specialized:
    // Assume a specialized conformance is fully applied.
    return getType();
  }
}

GenericSignature *ProtocolConformance::getGenericSignature() const {
  // FIXME: Should be an independent property of the conformance.
  // Assuming a BoundGenericType conformance is always for the
  // DeclaredTypeInContext is unsound if we ever add constrained extensions.

  return getType()->getNominalOrBoundGenericNominal()
    ->getGenericSignatureOfContext();
}

const Substitution &NormalProtocolConformance::getTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  auto known = TypeWitnesses.find(assocType);
  if (known == TypeWitnesses.end()) {
    assert(resolver && "Unable to resolve type witness");
    resolver->resolveTypeWitness(this, assocType);
    known = TypeWitnesses.find(assocType);
    assert(known != TypeWitnesses.end() && "Didn't resolve witness?");
  }

  return known->second;
}

void NormalProtocolConformance::setTypeWitness(
       AssociatedTypeDecl *assocType,
       const Substitution &substitution) const {
  assert(getProtocol() == cast<ProtocolDecl>(assocType->getDeclContext()) &&
         "associated type in wrong protocol");
  assert(TypeWitnesses.count(assocType) == 0 && "Type witness already known");
  assert(!isComplete() && "Conformance already complete?");
  TypeWitnesses[assocType] = substitution;
}

  /// Retrieve the value witness corresponding to the given requirement.
ConcreteDeclRef NormalProtocolConformance::getWitness(
                  ValueDecl *requirement, 
                  LazyResolver *resolver) const {
    assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
    auto known = Mapping.find(requirement);
    if (known == Mapping.end()) {
      assert(resolver && "Unable to resolve witness without resolver");
      resolver->resolveWitness(this, requirement);
      known = Mapping.find(requirement);
      assert(known != Mapping.end() && "Resolver did not resolve requirement");
    }

    return known->second;
  }

void NormalProtocolConformance::setWitness(ValueDecl *requirement,
                                           ConcreteDeclRef witness) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(getProtocol() == cast<ProtocolDecl>(requirement->getDeclContext()) &&
         "requirement in wrong protocol");
  assert(Mapping.count(requirement) == 0 && "Witness already known");
  assert(!isComplete() && "Conformance already complete?");
  Mapping[requirement] = witness;
}

const Substitution &SpecializedProtocolConformance::getTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  // If we've already created this type witness, return it.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end()) {
    return known->second;
  }

  // Otherwise, perform substitutions to create this witness now.
  TypeSubstitutionMap substitutionMap;
  for (const auto &substitution : GenericSubstitutions) {
    substitutionMap[substitution.Archetype] = substitution.Replacement;
  }

  auto &genericWitness = GenericConformance->getTypeWitness(assocType, resolver);
  auto conformingDC = getDeclContext();
  auto conformingModule = conformingDC->getParentModule();
  auto specializedType
    = genericWitness.Replacement.subst(conformingModule,
                                       substitutionMap,
                                       /*ignoreMissing=*/false,
                                       resolver);

  // If the type witness was unchanged, just copy it directly.
  if (specializedType.getPointer() == genericWitness.Replacement.getPointer()) {
    TypeWitnesses[assocType] = genericWitness;
    return TypeWitnesses[assocType];
  }

  // Gather the conformances for the type witness. These should never fail.
  SmallVector<ProtocolConformance *, 4> conformances;
  auto archetype = genericWitness.Archetype;
  for (auto proto : archetype->getConformsTo()) {
    auto conforms = conformingModule->lookupConformance(specializedType, proto,
                                                    resolver);
    assert(conforms.getInt() == ConformanceKind::Conforms &&
           "Improperly checked substitution");
    conformances.push_back(conforms.getPointer());
  }

  // Form the substitution.
  auto &ctx = assocType->getASTContext();
  TypeWitnesses[assocType] = Substitution{archetype, specializedType,
                                          ctx.AllocateCopy(conformances)};
  return TypeWitnesses[assocType];
}

ConcreteDeclRef
SpecializedProtocolConformance::getWitness(ValueDecl *requirement,
                                           LazyResolver *resolver) const {
  // FIXME: Apply substitutions here!
  return GenericConformance->getWitness(requirement, resolver);
}

const NormalProtocolConformance *
ProtocolConformance::getRootNormalConformance() const {
  const ProtocolConformance *C = this;
  while (!isa<NormalProtocolConformance>(C)) {
    switch (C->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have broken out of loop");
    case ProtocolConformanceKind::Inherited:
      C = cast<InheritedProtocolConformance>(C)
          ->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized:
      C = cast<SpecializedProtocolConformance>(C)
        ->getGenericConformance();
      break;
    }
  }
  return cast<NormalProtocolConformance>(C);
}

bool ProtocolConformance::isInheritable(LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(isInheritable, (resolver))
}

ProtocolConformance *ProtocolConformance::subst(Module *module,
                                                Type substType,
                                                ArrayRef<Substitution> subs,
                                                TypeSubstitutionMap &subMap) {
  if (getType()->isEqual(substType)) return this;
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    if (substType->isSpecialized()) {
      assert(getType()->isSpecialized()
             && "substitution mapped non-specialized to specialized?!");
      assert(getType()->getNominalOrBoundGenericNominal()
               == substType->getNominalOrBoundGenericNominal()
             && "substitution mapped to different nominal?!");
      return module->getASTContext()
        .getSpecializedConformance(substType, this,
                           substType->gatherAllSubstitutions(module, nullptr));
    }
    assert(substType->isEqual(getType())
           && "substitution changed non-specialized type?!");
    return this;
      
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    ProtocolConformance *newBase
      = cast<InheritedProtocolConformance>(this)->getInheritedConformance()
        ->subst(module, substType, subs, subMap);
    return module->getASTContext()
      .getInheritedConformance(substType, newBase);
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    SmallVector<Substitution, 8> newSubs;
    newSubs.reserve(spec->getGenericSubstitutions().size());
    for (auto &sub : spec->getGenericSubstitutions())
      newSubs.push_back(sub.subst(module, subs, subMap));
    
    auto ctxNewSubs = module->getASTContext().AllocateCopy(newSubs);
    
    return module->getASTContext()
      .getSpecializedConformance(substType, spec->getGenericConformance(),
                                 ctxNewSubs);
  }
  }
}

ProtocolConformance *
ProtocolConformance::getInheritedConformance(ProtocolDecl *protocol) const {
  // Preserve specialization through this operation by peeling off the
  // substitutions from a specialized conformance so we can apply them later.
  const ProtocolConformance *unspecialized;
  ArrayRef<Substitution> subs;
  switch (getKind()) {
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    unspecialized = spec->getGenericConformance();
    subs = spec->getGenericSubstitutions();
    break;
  }
    
  case ProtocolConformanceKind::Normal:
  case ProtocolConformanceKind::Inherited:
    unspecialized = this;
    break;
  }
  
  
  ProtocolConformance *foundInherited;
  
  // Search for the inherited conformance among our immediate parents.
  auto &inherited = unspecialized->getInheritedConformances();
  auto known = inherited.find(protocol);
  if (known != inherited.end()) {
    foundInherited = known->second;
    goto found_inherited;
  }

  // If not there, the inherited conformance must be available through one of
  // our parents.
  for (auto &inheritedMapping : inherited)
    if (inheritedMapping.first->inheritsFrom(protocol)) {
      foundInherited = inheritedMapping.second->
        getInheritedConformance(protocol);
      goto found_inherited;
    }

  llvm_unreachable("Can't find the inherited conformance.");

found_inherited:
  
  // Specialize the inherited conformance, if necessary.
  if (!subs.empty()) {
    return getType()->getASTContext()
      .getSpecializedConformance(getType(), foundInherited, subs);
  }
  assert(getType()->isEqual(foundInherited->getType())
         && "inherited conformance does not match type");
  return foundInherited;
}

namespace {
  /// Describes whether a requirement refers to 'Self', for use in the
  /// is-inheritable check.
  enum class SelfReferenceKind {
    /// The type does not refer to 'Self' at all.
    No,
    /// The type refers to 'Self', but only as the result type of a method.
    Result,
    /// The type refers to 'Self' in some position that is not the result type
    /// of a method.
    Yes
  };
}

/// Determine whether the given type is the 'Self' generic parameter
/// of a protocol.
static bool isSelf(Type type) {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    return genericParam->getDepth() == 0 && genericParam->getIndex() == 0;
  }

  return false;
}

/// Determine whether the given type contains a reference to the
/// 'Self' generic parameter of a protocol that is not the base of a
/// dependent member expression.
static bool containsSelf(Type type) {
  struct SelfWalker : public TypeWalker {
    bool FoundSelf = false;

    virtual Action walkToTypePre(Type ty) { 
      // If we found a reference to 'Self', note it and stop.
      if (isSelf(ty)) {
        FoundSelf = true;
        return Action::Stop;
      }

      // Don't recurse into the base of a dependent member type: it
      // doesn't contain a bare 'Self'.
      if (ty->is<DependentMemberType>())
        return Action::SkipChildren;

      return Action::Continue; 
    }
  } selfWalker;

  type.walk(selfWalker);
  return selfWalker.FoundSelf;
}

/// Find the bare Self references within the given requirement.
static SelfReferenceKind findSelfReferences(ValueDecl *value) {
  // Types never refer to 'Self'.
  if (isa<TypeDecl>(value))
    return SelfReferenceKind::No;

  // If the function requirement returns Self and has no other
  // reference to Self, note that.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    auto type = func->getInterfaceType();
    // Skip the 'self' type.
    type = type->castTo<AnyFunctionType>()->getResult();
    for (unsigned i = 1, n = func->getNumParamPatterns(); i != n; ++i) {
      // Check whether the input type contains Self anywhere.
      auto fnType = type->castTo<AnyFunctionType>();
      if (containsSelf(fnType->getInput()))
        return SelfReferenceKind::Yes;

      type = fnType->getResult();
    }

    // The result type remains.
    return (isSelf(type) || func->hasDynamicSelf())
             ? SelfReferenceKind::Result
             : containsSelf(type) ? SelfReferenceKind::Yes
                                  : SelfReferenceKind::No;
  }

  auto type = value->getInterfaceType();
  if (isa<ConstructorDecl>(value))
    type = type->castTo<AnyFunctionType>()->getResult();

  return containsSelf(type) ? SelfReferenceKind::Yes
                            : SelfReferenceKind::No;
}

bool NormalProtocolConformance::isInheritableSlow(LazyResolver *resolver) const{
  auto classDecl = getType()->getClassOrBoundGenericClass();
  assert(classDecl && "Conformance can't be inheritable, ever");
  
  for (auto member : getProtocol()->getMembers()) {
    auto req = dyn_cast<ValueDecl>(member);
    if (!req)
      continue;
    
    // Skip accessors.
    if (isa<FuncDecl>(req) && cast<FuncDecl>(req)->isAccessor())
      continue;

    // A non-abstract initializer witness makes the conformance non-inheritable.
    if (isa<ConstructorDecl>(req)) {
      auto ctorWitness = cast_or_null<ConstructorDecl>(
                           getWitness(req, resolver).getDecl());
      if (ctorWitness && !ctorWitness->isAbstract()) {
        DCAndInheritable.setInt(IsInheritableKind::NotInheritable);
        return false;
      }
    }

    // Check the kinds of references to Self that show up in the given
    // requirement.
    switch (findSelfReferences(req)) {
    case SelfReferenceKind::No:
      continue;

    case SelfReferenceKind::Result: {
      // When only the result type is 'Self', we can inherit the
      // conformance if the witness returns DynamicSelf.
      auto func = cast_or_null<FuncDecl>(
                    getWitness(req, resolver).getDecl());
      if (func && func->hasDynamicSelf())
        continue;

      // Fall through
    }

    case SelfReferenceKind::Yes: {
      DCAndInheritable.setInt(IsInheritableKind::NotInheritable);
      return false;
    }
    }
  }

  // Note that this conformance is inheritable.
  DCAndInheritable.setInt(IsInheritableKind::Inheritable);
  return true;
}
