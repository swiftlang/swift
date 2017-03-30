//===--- SILGenType.cpp - SILGen for types and their members --------------===//
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
// This file contains code for emitting code associated with types:
//   - methods
//   - vtables and vtable thunks
//   - witness tables and witness thunks
//
//===----------------------------------------------------------------------===//

#include "SILGenFunction.h"
#include "Scope.h"
#include "ManagedValue.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;


SILVTable::Entry
SILGenModule::emitVTableMethod(SILDeclRef derived, SILDeclRef base) {
  assert(base.kind == derived.kind);

  SILFunction *implFn;
  SILLinkage implLinkage;

  // If the member is dynamic, reference its dynamic dispatch thunk so that
  // it will be redispatched, funneling the method call through the runtime
  // hook point.
  if (derived.getDecl()->isDynamic()) {
    implFn = getDynamicThunk(derived, Types.getConstantInfo(derived));
    implLinkage = SILLinkage::Public;
  } else {
    implFn = getFunction(derived, NotForDefinition);
    implLinkage = stripExternalFromLinkage(implFn->getLinkage());
  }

  // As a fast path, if there is no override, definitely no thunk is necessary.
  if (derived == base)
    return {base, implFn, implLinkage};

  // Determine the derived thunk type by lowering the derived type against the
  // abstraction pattern of the base.
  auto baseInfo = Types.getConstantInfo(base);
  auto derivedInfo = Types.getConstantInfo(derived);
  auto basePattern = AbstractionPattern(baseInfo.LoweredInterfaceType);
  
  auto overrideInfo = M.Types.getConstantOverrideInfo(derived, base);

  // The override member type is semantically a subtype of the base
  // member type. If the override is ABI compatible, we do not need
  // a thunk.
  if (M.Types.checkFunctionForABIDifferences(derivedInfo.SILFnType,
                                             overrideInfo.SILFnType)
      == TypeConverter::ABIDifference::Trivial)
    return {base, implFn, implLinkage};

  // Generate the thunk name.
  std::string name;
  {
    Mangle::ASTMangler mangler;
    if (isa<FuncDecl>(base.getDecl())) {
      name = mangler.mangleVTableThunk(
        cast<FuncDecl>(base.getDecl()),
        cast<FuncDecl>(derived.getDecl()));
    } else {
      name = mangler.mangleConstructorVTableThunk(
        cast<ConstructorDecl>(base.getDecl()),
        cast<ConstructorDecl>(derived.getDecl()),
        base.kind == SILDeclRef::Kind::Allocator);
    }
  }

  // If we already emitted this thunk, reuse it.
  if (auto existingThunk = M.lookUpFunction(name))
    return {base, existingThunk, implLinkage};

  // Emit the thunk.
  auto *derivedDecl = cast<AbstractFunctionDecl>(derived.getDecl());
  SILLocation loc(derivedDecl);
  auto thunk =
      M.createFunction(SILLinkage::Private,
                       name, overrideInfo.SILFnType,
                       derivedDecl->getGenericEnvironment(), loc, IsBare,
                       IsNotTransparent, IsNotSerialized);
  thunk->setDebugScope(new (M) SILDebugScope(loc, thunk));

  SILGenFunction(*this, *thunk)
    .emitVTableThunk(derived, implFn, basePattern,
                     overrideInfo.LoweredInterfaceType,
                     derivedInfo.LoweredInterfaceType);

  return {base, thunk, implLinkage};
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property unless
  // the @NSManaged attribute is present.
  if (method->isGetterOrSetter()) {
    auto asd = method->getAccessorStorageDecl();
    return asd->isObjC() && !asd->getAttrs().hasAttribute<NSManagedAttr>();
  }

  if (method->getAttrs().hasAttribute<NSManagedAttr>())
    return false;

  return method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>();
}

bool SILGenModule::requiresObjCMethodEntryPoint(ConstructorDecl *constructor) {
  return constructor->isObjC();
}

namespace {

/// An ASTVisitor for populating SILVTable entries from ClassDecl members.
class SILGenVTable : public SILVTableVisitor<SILGenVTable> {
public:
  SILGenModule &SGM;
  ClassDecl *theClass;

  // Map a base SILDeclRef to the corresponding element in vtableMethods.
  llvm::DenseMap<SILDeclRef, unsigned> baseToIndexMap;

  // For each base method, store the corresponding override.
  SmallVector<std::pair<SILDeclRef, SILDeclRef>, 8> vtableMethods;

  SILGenVTable(SILGenModule &SGM, ClassDecl *theClass)
    : SILVTableVisitor(SGM.Types), SGM(SGM), theClass(theClass)
  { }

  void emitVTable() {
    // Populate the superclass members, if any.
    visitAncestor(theClass);

    auto *dtor = theClass->getDestructor();
    assert(dtor);

    // Add the deallocating destructor to the vtable just for the purpose
    // that it is referenced and cannot be eliminated by dead function removal.
    // In reality, the deallocating destructor is referenced directly from
    // the HeapMetadata for the class.
    if (!dtor->hasClangNode())
      addMethod(SILDeclRef(dtor, SILDeclRef::Kind::Deallocator));

    if (SGM.requiresIVarDestroyer(theClass))
      addMethod(SILDeclRef(theClass, SILDeclRef::Kind::IVarDestroyer));

    SmallVector<SILVTable::Entry, 8> vtableEntries;
    vtableEntries.reserve(vtableMethods.size());

    for (auto method : vtableMethods) {
      SILDeclRef baseRef, derivedRef;
      std::tie(baseRef, derivedRef) = method;

      vtableEntries.push_back(SGM.emitVTableMethod(derivedRef, baseRef));
    }

    // Create the vtable.
    SILVTable::create(SGM.M, theClass, vtableEntries);
  }

  void visitAncestor(ClassDecl *ancestor) {
    auto superTy = ancestor->getSuperclass();
    if (superTy)
      visitAncestor(superTy->getClassOrBoundGenericClass());

    addVTableEntries(ancestor);
  }

  // Try to find an overridden entry.
  void addMethodOverride(SILDeclRef baseRef, SILDeclRef declRef) {
    auto found = baseToIndexMap.find(baseRef);
    assert(found != baseToIndexMap.end());
    auto &method = vtableMethods[found->second];
    assert(method.first == baseRef);
    method.second = declRef;
  }

  // Add an entry to the vtable.
  void addMethod(SILDeclRef member) {
    unsigned index = vtableMethods.size();
    vtableMethods.push_back(std::make_pair(member, member));
    auto result = baseToIndexMap.insert(std::make_pair(member, index));
    assert(result.second);
    (void) result;
  }
};

}

static void emitTypeMemberGlobalVariable(SILGenModule &SGM,
                                         VarDecl *var) {
  if (var->getDeclContext()->isGenericContext()) {
    assert(var->getDeclContext()->getGenericSignatureOfContext()
              ->areAllParamsConcrete()
           && "generic static vars are not implemented yet");
  }

  if (var->getDeclContext()->getAsClassOrClassExtensionContext()) {
    assert(var->isFinal() && "only 'static' ('class final') stored properties are implemented in classes");
  }

  SGM.addGlobalVariable(var);
}

namespace {

// Is this a free function witness satisfying a static method requirement?
static IsFreeFunctionWitness_t isFreeFunctionWitness(ValueDecl *requirement,
                                                     ValueDecl *witness) {
  if (!witness->getDeclContext()->isTypeContext()) {
    assert(!requirement->isInstanceMember()
           && "free function satisfying instance method requirement?!");
    return IsFreeFunctionWitness;
  }

  return IsNotFreeFunctionWitness;
}

/// A CRTP class for emitting witness thunks for the requirements of a
/// protocol.
///
/// There are two subclasses:
///
/// - SILGenConformance: emits witness thunks for a conformance of a
///   a concrete type to a protocol
/// - SILGenDefaultWitnessTable: emits default witness thunks for
///   default implementations of protocol requirements
///
template<typename T> class SILGenWitnessTable : public SILWitnessVisitor<T> {
  T &asDerived() { return *static_cast<T*>(this); }

public:
  void addMethod(FuncDecl *fd, Witness witness) {
    return addMethod(fd, witness.getDecl(), witness);
  }

  void addConstructor(ConstructorDecl *cd, Witness witness) {
    SILDeclRef requirementRef(cd, SILDeclRef::Kind::Allocator,
                              ResilienceExpansion::Minimal);

    SILDeclRef witnessRef(witness.getDecl(), SILDeclRef::Kind::Allocator,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          requirementRef.uncurryLevel);

    asDerived().addMethod(requirementRef, witnessRef, IsNotFreeFunctionWitness,
                          witness);
  }

  /// Subclasses must override SILWitnessVisitor::visitAbstractStorageDecl()
  /// to call addAbstractStorageDecl(), since we need the substitutions to
  /// be passed down into addMethod().
  ///
  /// FIXME: Seems that conformance->getWitness() should do this for us?
  void addAbstractStorageDecl(AbstractStorageDecl *d, Witness witness) {
    auto *witnessSD = cast<AbstractStorageDecl>(witness.getDecl());
    addMethod(d->getGetter(), witnessSD->getGetter(), witness);
    if (d->isSettable(d->getDeclContext()))
      addMethod(d->getSetter(), witnessSD->getSetter(), witness);
    if (auto materializeForSet = d->getMaterializeForSetFunc())
      addMethod(materializeForSet, witnessSD->getMaterializeForSetFunc(),
                witness);
  }

private:
  void addMethod(FuncDecl *fd, ValueDecl *witnessDecl, Witness witness) {

    // TODO: multiple resilience expansions?
    // TODO: multiple uncurry levels?
    SILDeclRef requirementRef(fd, SILDeclRef::Kind::Func,
                              ResilienceExpansion::Minimal);
    // Free function witnesses have an implicit uncurry layer imposed on them by
    // the inserted metatype argument.
    auto isFree = isFreeFunctionWitness(fd, witnessDecl);
    unsigned witnessUncurryLevel = isFree ? requirementRef.uncurryLevel - 1
                                          : requirementRef.uncurryLevel;

    SILDeclRef witnessRef(witnessDecl, SILDeclRef::Kind::Func,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          witnessUncurryLevel);

    asDerived().addMethod(requirementRef, witnessRef, isFree, witness);
  }

};

/// Emit a witness table for a protocol conformance.
class SILGenConformance : public SILGenWitnessTable<SILGenConformance> {
  using super = SILGenWitnessTable<SILGenConformance>;

public:
  SILGenModule &SGM;
  NormalProtocolConformance *Conformance;
  std::vector<SILWitnessTable::Entry> Entries;
  SILLinkage Linkage;

  SILGenConformance(SILGenModule &SGM, NormalProtocolConformance *C)
    // We only need to emit witness tables for base NormalProtocolConformances.
    : SGM(SGM), Conformance(C->getRootNormalConformance()),
      Linkage(getLinkageForProtocolConformance(Conformance,
                                               ForDefinition))
  {
    // Not all protocols use witness tables.
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(
        Conformance->getProtocol()))
      Conformance = nullptr;
  }

  SILWitnessTable *emit() {
    // Nothing to do if this wasn't a normal conformance.
    if (!Conformance)
      return nullptr;

    auto *proto = Conformance->getProtocol();
    visitProtocolDecl(proto);

    // Serialize the witness table in two cases:
    // 1) We're serializing everything
    // 2) The type has a fixed layout in all resilience domains, and the
    //    conformance is externally visible
    IsSerialized_t isSerialized = IsNotSerialized;
    if (SGM.makeModuleFragile)
      isSerialized = IsSerialized;
    if (auto nominal = Conformance->getInterfaceType()->getAnyNominal())
      if (nominal->hasFixedLayout() &&
          proto->getEffectiveAccess() >= Accessibility::Public &&
          nominal->getEffectiveAccess() >= Accessibility::Public)
        isSerialized = IsSerialized;

    // Check if we already have a declaration or definition for this witness
    // table.
    if (auto *wt = SGM.M.lookUpWitnessTable(Conformance, false)) {
      // If we have a definition already, just return it.
      //
      // FIXME: I am not sure if this is possible, if it is not change this to an
      // assert.
      if (wt->isDefinition())
        return wt;

      // If we have a declaration, convert the witness table to a definition.
      if (wt->isDeclaration()) {
        wt->convertToDefinition(Entries, isSerialized);

        // Since we had a declaration before, its linkage should be external,
        // ensure that we have a compatible linkage for sanity. *NOTE* we are ok
        // with both being shared since we do not have a shared_external
        // linkage.
        assert(stripExternalFromLinkage(wt->getLinkage()) == Linkage &&
               "Witness table declaration has inconsistent linkage with"
               " silgen definition.");

        // And then override the linkage with the new linkage.
        wt->setLinkage(Linkage);
        return wt;
      }
    }

    // Otherwise if we have no witness table yet, create it.
    return SILWitnessTable::create(SGM.M, Linkage, isSerialized,
                                   Conformance, Entries);
  }

  void addOutOfLineBaseProtocol(ProtocolDecl *baseProtocol) {
    assert(Lowering::TypeConverter::protocolRequiresWitnessTable(baseProtocol));

    auto foundBaseConformance
      = Conformance->getInheritedConformances().find(baseProtocol);
    assert(foundBaseConformance != Conformance->getInheritedConformances().end()
           && "no inherited conformance for base protocol");

    auto conformance = foundBaseConformance->second;

    Entries.push_back(SILWitnessTable::BaseProtocolWitness{
      baseProtocol,
      conformance,
    });

    // Emit the witness table for the base conformance if it is shared.
    if (getLinkageForProtocolConformance(
                                        conformance->getRootNormalConformance(),
                                        NotForDefinition)
          == SILLinkage::Shared)
      SGM.getWitnessTable(conformance->getRootNormalConformance());
  }

  void addMethod(FuncDecl *fd) {
    Witness witness = Conformance->getWitness(fd, nullptr);
    super::addMethod(fd, witness);
  }

  void addConstructor(ConstructorDecl *cd) {
    Witness witness = Conformance->getWitness(cd, nullptr);
    super::addConstructor(cd, witness);
  }

  void addMethod(SILDeclRef requirementRef,
                 SILDeclRef witnessRef,
                 IsFreeFunctionWitness_t isFree,
                 Witness witness) {
    // Emit the witness thunk and add it to the table.

    // If this is a non-present optional requirement, emit a MissingOptional.
    if (!witnessRef) {
      auto *fd = requirementRef.getDecl();
      assert(fd->getAttrs().hasAttribute<OptionalAttr>() &&
             "Non-optional protocol requirement lacks a witness?");
      Entries.push_back(SILWitnessTable::MissingOptionalWitness{ fd });
      return;
    }

    SILFunction *witnessFn =
      SGM.emitProtocolWitness(Conformance, Linkage, requirementRef, witnessRef,
                              isFree, witness);
    Entries.push_back(
                    SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void addAssociatedType(AssociatedTypeDecl *td) {
    // Find the substitution info for the witness type.
    const auto &witness = Conformance->getTypeWitness(td, /*resolver=*/nullptr);

    // Emit the record for the type itself.
    Entries.push_back(SILWitnessTable::AssociatedTypeWitness{td,
                                witness.getReplacement()->getCanonicalType()});    
  }

  void addAssociatedConformance(CanType dependentType, ProtocolDecl *protocol) {
    auto assocConformance =
      Conformance->getAssociatedConformance(dependentType, protocol);

    SGM.useConformance(assocConformance);

    Entries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        dependentType, protocol, assocConformance});
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *d) {
    Witness witness = Conformance->getWitness(d, nullptr);
    addAbstractStorageDecl(d, witness);
  }
};

} // end anonymous namespace

static SILWitnessTable *
getWitnessTableToInsertAfter(SILGenModule &SGM,
                             NormalProtocolConformance *insertAfter) {
  while (insertAfter) {
    // If the table was emitted, emit after it.
    auto found = SGM.emittedWitnessTables.find(insertAfter);
    if (found != SGM.emittedWitnessTables.end())
      return found->second;

    // Otherwise, try inserting after the table we would transitively be
    // inserted after.
    auto foundDelayed = SGM.delayedConformances.find(insertAfter);
    if (foundDelayed != SGM.delayedConformances.end())
      insertAfter = foundDelayed->second.insertAfter;
    else
      break;
  }

  return nullptr;
}

SILWitnessTable *
SILGenModule::getWitnessTable(ProtocolConformance *conformance) {
  auto normal = conformance->getRootNormalConformance();

  // If we've already emitted this witness table, return it.
  auto found = emittedWitnessTables.find(normal);
  if (found != emittedWitnessTables.end())
    return found->second;

  SILWitnessTable *table = SILGenConformance(*this, normal).emit();
  emittedWitnessTables.insert({normal, table});

  // If we delayed emission of this witness table, move it to its rightful
  // place within the module.
  auto foundDelayed = delayedConformances.find(normal);
  if (foundDelayed != delayedConformances.end()) {
    M.witnessTables.remove(table);
    auto insertAfter = getWitnessTableToInsertAfter(*this,
                                              foundDelayed->second.insertAfter);
    if (!insertAfter) {
      M.witnessTables.push_front(table);
    } else {
      M.witnessTables.insertAfter(insertAfter->getIterator(), table);
    }
  } else {
    // We would have marked a delayed conformance as "last emitted" when it
    // was delayed.
    lastEmittedConformance = normal;
  }
  return table;
}

static bool maybeOpenCodeProtocolWitness(SILGenFunction &gen,
                                         ProtocolConformance *conformance,
                                         SILLinkage linkage,
                                         Type selfInterfaceType,
                                         Type selfType,
                                         GenericEnvironment *genericEnv,
                                         SILDeclRef requirement,
                                         SILDeclRef witness,
                                         SubstitutionList witnessSubs) {
  if (auto witnessFn = dyn_cast<FuncDecl>(witness.getDecl())) {
    if (witnessFn->getAccessorKind() == AccessorKind::IsMaterializeForSet) {
      auto reqFn = cast<FuncDecl>(requirement.getDecl());
      assert(reqFn->getAccessorKind() == AccessorKind::IsMaterializeForSet);
      return gen.maybeEmitMaterializeForSetThunk(conformance, linkage,
                                                 selfInterfaceType, selfType,
                                                 genericEnv, reqFn, witnessFn,
                                                 witnessSubs);
    }
  }

  return false;
}

SILFunction *
SILGenModule::emitProtocolWitness(ProtocolConformance *conformance,
                                  SILLinkage linkage,
                                  SILDeclRef requirement,
                                  SILDeclRef witnessRef,
                                  IsFreeFunctionWitness_t isFree,
                                  Witness witness) {
  auto requirementInfo = Types.getConstantInfo(requirement);
  unsigned witnessUncurryLevel = witnessRef.uncurryLevel;

  // If the witness is a free function, consider the self argument
  // uncurry level.
  if (isFree)
    ++witnessUncurryLevel;

  // The SIL witness thunk has the type of the AST-level witness with
  // witness substitutions applied, at the abstraction level of the
  // original protocol requirement.
  assert(requirement.uncurryLevel == witnessUncurryLevel &&
         "uncurry level of requirement and witness do not match");

  GenericEnvironment *genericEnv = nullptr;

  // Work out the lowered function type of the SIL witness thunk.
  auto reqtOrigTy
    = cast<GenericFunctionType>(requirementInfo.LoweredInterfaceType);
  CanAnyFunctionType reqtSubstTy;
  SubstitutionList witnessSubs;
  if (witness.requiresSubstitution()) {
    genericEnv = witness.getSyntheticEnvironment();
    witnessSubs = witness.getSubstitutions();

    auto reqtSubs = witness.getRequirementToSyntheticSubs();
    auto reqtSubMap = reqtOrigTy->getGenericSignature()
        ->getSubstitutionMap(reqtSubs);
    auto input = reqtOrigTy->getInput().subst(reqtSubMap);
    auto result = reqtOrigTy->getResult().subst(reqtSubMap);

    if (genericEnv) {
      auto *genericSig = genericEnv->getGenericSignature();
      reqtSubstTy = cast<GenericFunctionType>(
        GenericFunctionType::get(genericSig, input, result,
                                 reqtOrigTy->getExtInfo())
          ->getCanonicalType());
    } else {
      reqtSubstTy = cast<FunctionType>(
        FunctionType::get(input, result,
                          reqtOrigTy->getExtInfo())
          ->getCanonicalType());
    }
  } else {
    genericEnv = witnessRef.getDecl()->getInnermostDeclContext()
                   ->getGenericEnvironmentOfContext();

    Type concreteTy = conformance->getInterfaceType();

    // FIXME: conformance substitutions should be in terms of interface types
    auto specialized = conformance;
    if (conformance->getGenericSignature()) {
      ASTContext &ctx = getASTContext();

      auto concreteSubs = concreteTy->getContextSubstitutionMap(
          M.getSwiftModule(),
          conformance->getDeclContext());
      specialized = ctx.getSpecializedConformance(concreteTy, conformance,
                                                  concreteSubs);
    }

    auto reqtSubs = SubstitutionMap::getProtocolSubstitutions(
        conformance->getProtocol(),
        concreteTy,
        ProtocolConformanceRef(specialized));

    auto input = reqtOrigTy->getInput().subst(reqtSubs)->getCanonicalType();
    auto result = reqtOrigTy->getResult().subst(reqtSubs)->getCanonicalType();

    reqtSubstTy = CanFunctionType::get(input, result, reqtOrigTy->getExtInfo());
  }

  // Lower the witness thunk type with the requirement's abstraction level.
  auto witnessSILFnType = getNativeSILFunctionType(M,
                                                   AbstractionPattern(reqtOrigTy),
                                                   reqtSubstTy,
                                                   witnessRef);

  // Mangle the name of the witness thunk.
  Mangle::ASTMangler NewMangler;
  std::string nameBuffer = NewMangler.mangleWitnessThunk(conformance,
                                                         requirement.getDecl());
  
  // If the thunked-to function is set to be always inlined, do the
  // same with the witness, on the theory that the user wants all
  // calls removed if possible, e.g. when we're able to devirtualize
  // the witness method call. Otherwise, use the default inlining
  // setting on the theory that forcing inlining off should only
  // effect the user's function, not otherwise invisible thunks.
  Inline_t InlineStrategy = InlineDefault;
  if (witnessRef.isAlwaysInline())
    InlineStrategy = AlwaysInline;

  IsSerialized_t isSerialized = IsNotSerialized;
  if (makeModuleFragile)
    isSerialized = IsSerialized;
  if (witnessRef.isSerialized())
    isSerialized = IsSerialized;

  auto *f = M.createFunction(
      linkage, nameBuffer, witnessSILFnType,
      genericEnv, SILLocation(witnessRef.getDecl()),
      IsNotBare, IsTransparent, isSerialized, IsThunk,
      SILFunction::NotRelevant, InlineStrategy);

  f->setDebugScope(new (M)
                   SILDebugScope(RegularLocation(witnessRef.getDecl()), f));

  PrettyStackTraceSILFunction trace("generating protocol witness thunk", f);

  // Create the witness.
  Type selfInterfaceType;
  Type selfType;

  // If the witness is a free function, there is no Self type.
  if (!isFree) {
    if (conformance) {
      selfInterfaceType = conformance->getInterfaceType();
    } else {
      auto *proto = cast<ProtocolDecl>(requirement.getDecl()->getDeclContext());
      selfInterfaceType = proto->getSelfInterfaceType();
    }

    selfType = GenericEnvironment::mapTypeIntoContext(
        genericEnv, selfInterfaceType);
  }

  SILGenFunction gen(*this, *f);

  // Open-code certain protocol witness "thunks".
  if (maybeOpenCodeProtocolWitness(gen, conformance, linkage,
                                   selfInterfaceType, selfType, genericEnv,
                                   requirement, witnessRef, witnessSubs)) {
    assert(!isFree);
    return f;
  }

  gen.emitProtocolWitness(selfType,
                          AbstractionPattern(reqtOrigTy),
                          reqtSubstTy,
                          requirement, witnessRef,
                          witnessSubs, isFree);

  return f;
}

namespace {

/// Emit a default witness table for a resilient protocol definition.
class SILGenDefaultWitnessTable
    : public SILGenWitnessTable<SILGenDefaultWitnessTable> {
  using super = SILGenWitnessTable<SILGenDefaultWitnessTable>;

public:
  SILGenModule &SGM;
  ProtocolDecl *Proto;
  SILLinkage Linkage;

  SmallVector<SILDefaultWitnessTable::Entry, 8> DefaultWitnesses;

  SILGenDefaultWitnessTable(SILGenModule &SGM, ProtocolDecl *proto,
                            SILLinkage linkage)
      : SGM(SGM), Proto(proto), Linkage(linkage) { }

  void addMissingDefault() {
    DefaultWitnesses.push_back(SILDefaultWitnessTable::Entry());
  }

  void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
    addMissingDefault();
  }

  void addMethod(FuncDecl *fd) {
    auto witness = Proto->getDefaultWitness(fd);
    if (!witness) {
      addMissingDefault();
      return;
    }

    super::addMethod(fd, witness);
  }

  void addConstructor(ConstructorDecl *cd) {
    auto witness = Proto->getDefaultWitness(cd);
    if (!witness) {
      addMissingDefault();
      return;
    }

    super::addConstructor(cd, witness);
  }

  void addMethod(SILDeclRef requirementRef,
                 SILDeclRef witnessRef,
                 IsFreeFunctionWitness_t isFree,
                 Witness witness) {
    SILFunction *witnessFn = SGM.emitProtocolWitness(nullptr, Linkage,
                                                     requirementRef, witnessRef,
                                                     isFree, witness);
    auto entry = SILDefaultWitnessTable::Entry(requirementRef, witnessFn);
    DefaultWitnesses.push_back(entry);
  }

  void addAssociatedType(AssociatedTypeDecl *ty) {
    // Add a dummy entry for the metatype itself.
    addMissingDefault();
  }

  void addAssociatedConformance(CanType type, ProtocolDecl *requirement) {
    addMissingDefault();
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *d) {
    auto witness = Proto->getDefaultWitness(d);
    if (!witness) {
      addMissingDefault();
      if (d->isSettable(d->getDeclContext()))
        addMissingDefault();
      if (d->getMaterializeForSetFunc())
        addMissingDefault();
      return;
    }

    addAbstractStorageDecl(d, witness);
  }
};

} // end anonymous namespace

void SILGenModule::emitDefaultWitnessTable(ProtocolDecl *protocol) {
  SILLinkage linkage =
      getSILLinkage(getDeclLinkage(protocol), ForDefinition);

  SILGenDefaultWitnessTable builder(*this, protocol, linkage);
  builder.visitProtocolDecl(protocol);

  SILDefaultWitnessTable *defaultWitnesses =
      M.createDefaultWitnessTableDeclaration(protocol, linkage);
  defaultWitnesses->convertToDefinition(builder.DefaultWitnesses);
}

namespace {

/// An ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class SILGenType : public TypeMemberVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;

  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
    : SGM(SGM), theType(theType) {}

  /// Emit SIL functions for all the members of the type.
  void emitType() {
    for (Decl *member : theType->getMembers())
      visit(member);

    // Build a vtable if this is a class.
    if (auto theClass = dyn_cast<ClassDecl>(theType)) {
      SILGenVTable genVTable(SGM, theClass);
      genVTable.emitVTable();
    }

    // Build a default witness table if this is a protocol.
    if (auto protocol = dyn_cast<ProtocolDecl>(theType)) {
      if (!protocol->isObjC())
        SGM.emitDefaultWitnessTable(protocol);
      return;
    }

    // Emit witness tables for conformances of concrete types. Protocol types
    // are existential and do not have witness tables.
    for (auto *conformance : theType->getLocalConformances(
                               ConformanceLookupKind::All,
                               nullptr, /*sorted=*/true)) {
      if (conformance->isComplete() &&
          isa<NormalProtocolConformance>(conformance))
        SGM.getWitnessTable(conformance);
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitModuleDecl(ModuleDecl *md) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCMethodEntryPoint(fd) &&
        !isa<ProtocolDecl>(fd->getDeclContext()))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);

    if (SGM.requiresObjCMethodEntryPoint(cd) &&
        !isa<ProtocolDecl>(cd->getDeclContext()))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    assert(isa<ClassDecl>(theType) && "destructor in a non-class type");
    ProfilerRAII Profiler(SGM, dd);
    SGM.emitDestructor(cast<ClassDecl>(theType), dd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ued) {}

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers.
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i) {
      if (pd->getInit(i)) {
        if (pd->isStatic())
          SGM.emitGlobalInitialization(pd, i);
        else
          SGM.emitStoredPropertyInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->hasBehavior())
      SGM.emitPropertyBehavior(vd);

    // Collect global variables for static properties.
    // FIXME: We can't statically emit a global variable for generic properties.
    if (vd->isStatic() && vd->hasStorage()) {
      return emitTypeMemberGlobalVariable(SGM, vd);
    }

    visitAbstractStorageDecl(vd);
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *asd) {
    // FIXME: Default implementations in protocols.
    if (asd->isObjC() && !isa<ProtocolDecl>(asd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(asd);
  }
};

} // end anonymous namespace

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this, ntd).emitType();
}

/// SILGenExtension - an ASTVisitor for generating SIL from method declarations
/// and protocol conformances inside type extensions.
class SILGenExtension : public TypeMemberVisitor<SILGenExtension> {
public:
  SILGenModule &SGM;

  SILGenExtension(SILGenModule &SGM)
    : SGM(SGM) {}

  /// Emit SIL functions for all the members of the extension.
  void emitExtension(ExtensionDecl *e) {
    for (Decl *member : e->getMembers())
      visit(member);

    if (!e->getExtendedType()->isExistentialType()) {
      // Emit witness tables for protocol conformances introduced by the
      // extension.
      for (auto *conformance : e->getLocalConformances(
                                 ConformanceLookupKind::All,
                                 nullptr, /*sorted=*/true)) {
        if (conformance->isComplete() &&
            isa<NormalProtocolConformance>(conformance))
          SGM.getWitnessTable(conformance);
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitModuleDecl(ModuleDecl *md) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    if (SGM.requiresObjCMethodEntryPoint(fd))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);
    if (SGM.requiresObjCMethodEntryPoint(cd))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    llvm_unreachable("destructor in extension?!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i) {
      if (pd->getInit(i)) {
        assert(pd->isStatic() && "stored property in extension?!");
        SGM.emitGlobalInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->hasBehavior())
      SGM.emitPropertyBehavior(vd);
    if (vd->hasStorage()) {
      assert(vd->isStatic() && "stored property in extension?!");
      return emitTypeMemberGlobalVariable(SGM, vd);
    }
    visitAbstractStorageDecl(vd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ed) {
    llvm_unreachable("enum elements aren't allowed in extensions");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *vd) {
    if (vd->isObjC())
      SGM.emitObjCPropertyMethodThunks(vd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}
