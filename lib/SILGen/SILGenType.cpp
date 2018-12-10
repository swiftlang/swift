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

#include "ManagedValue.h"
#include "SILGenFunction.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;


Optional<SILVTable::Entry>
SILGenModule::emitVTableMethod(ClassDecl *theClass,
                               SILDeclRef derived, SILDeclRef base) {
  assert(base.kind == derived.kind);

  auto *baseDecl = base.getDecl();
  auto *derivedDecl = derived.getDecl();

  // Note: We intentionally don't support extension members here.
  //
  // Once extensions can override or introduce new vtable entries, this will
  // all likely change anyway.
  auto *baseClass = cast<ClassDecl>(baseDecl->getDeclContext());
  auto *derivedClass = cast<ClassDecl>(derivedDecl->getDeclContext());

  // Figure out if the vtable entry comes from the superclass, in which
  // case we won't emit it if building a resilient module.
  SILVTable::Entry::Kind implKind;
  if (baseClass == theClass) {
    // This is a vtable entry for a method of the immediate class.
    implKind = SILVTable::Entry::Kind::Normal;
  } else if (derivedClass == theClass) {
    // This is a vtable entry for a method of a base class, but it is being
    // overridden in the immediate class.
    implKind = SILVTable::Entry::Kind::Override;
  } else {
    // This vtable entry is copied from the superclass.
    implKind = SILVTable::Entry::Kind::Inherited;

    // If the override is defined in a class from a different resilience
    // domain, don't emit the vtable entry.
    if (derivedClass->isResilient(M.getSwiftModule(),
                                  ResilienceExpansion::Maximal)) {
      return None;
    }
  }

  SILFunction *implFn;
  SILLinkage implLinkage;

  // If the member is dynamic, reference its dynamic dispatch thunk so that
  // it will be redispatched, funneling the method call through the runtime
  // hook point.
  if (derivedDecl->isObjCDynamic()
      && derived.kind != SILDeclRef::Kind::Allocator) {
    implFn = getDynamicThunk(derived, Types.getConstantInfo(derived).SILFnType);
    implLinkage = SILLinkage::Public;
  } else {
    implFn = getFunction(derived, NotForDefinition);
    implLinkage = stripExternalFromLinkage(implFn->getLinkage());
  }

  // As a fast path, if there is no override, definitely no thunk is necessary.
  if (derived == base)
    return SILVTable::Entry(base, implFn, implKind, implLinkage);

  // Determine the derived thunk type by lowering the derived type against the
  // abstraction pattern of the base.
  auto baseInfo = Types.getConstantInfo(base);
  auto derivedInfo = Types.getConstantInfo(derived);
  auto basePattern = AbstractionPattern(baseInfo.LoweredType);
  
  auto overrideInfo = M.Types.getConstantOverrideInfo(derived, base);

  // The override member type is semantically a subtype of the base
  // member type. If the override is ABI compatible, we do not need
  // a thunk.
  if (M.Types.checkFunctionForABIDifferences(derivedInfo.SILFnType,
                                             overrideInfo.SILFnType)
      == TypeConverter::ABIDifference::Trivial)
    return SILVTable::Entry(base, implFn, implKind, implLinkage);

  // Generate the thunk name.
  std::string name;
  {
    Mangle::ASTMangler mangler;
    if (isa<FuncDecl>(baseDecl)) {
      name = mangler.mangleVTableThunk(
        cast<FuncDecl>(baseDecl),
        cast<FuncDecl>(derivedDecl));
    } else {
      name = mangler.mangleConstructorVTableThunk(
        cast<ConstructorDecl>(baseDecl),
        cast<ConstructorDecl>(derivedDecl),
        base.kind == SILDeclRef::Kind::Allocator);
    }
  }

  // If we already emitted this thunk, reuse it.
  if (auto existingThunk = M.lookUpFunction(name))
    return SILVTable::Entry(base, existingThunk, implKind, implLinkage);

  // Emit the thunk.
  SILLocation loc(derivedDecl);
  SILGenFunctionBuilder builder(*this);
  auto thunk = builder.createFunction(
      SILLinkage::Private, name, overrideInfo.SILFnType,
      cast<AbstractFunctionDecl>(derivedDecl)->getGenericEnvironment(), loc,
      IsBare, IsNotTransparent, IsNotSerialized, IsNotDynamic);
  thunk->setDebugScope(new (M) SILDebugScope(loc, thunk));

  SILGenFunction(*this, *thunk, theClass)
    .emitVTableThunk(derived, implFn, basePattern,
                     overrideInfo.LoweredType,
                     derivedInfo.LoweredType);

  return SILVTable::Entry(base, thunk, implKind, implLinkage);
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property unless
  // the @NSManaged attribute is present.
  if (auto accessor = dyn_cast<AccessorDecl>(method)) {
    if (accessor->isGetterOrSetter()) {
      auto asd = accessor->getStorage();
      return asd->isObjC() && !asd->getAttrs().hasAttribute<NSManagedAttr>();
    }
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
  bool isResilient;

  // Map a base SILDeclRef to the corresponding element in vtableMethods.
  llvm::DenseMap<SILDeclRef, unsigned> baseToIndexMap;

  // For each base method, store the corresponding override.
  SmallVector<std::pair<SILDeclRef, SILDeclRef>, 8> vtableMethods;

  SILGenVTable(SILGenModule &SGM, ClassDecl *theClass)
    : SGM(SGM), theClass(theClass) {
    isResilient = theClass->isResilient();
  }

  void emitVTable() {
    // Imported types don't have vtables right now.
    if (theClass->hasClangNode())
      return;

    // Populate our list of base methods and overrides.
    visitAncestor(theClass);

    SmallVector<SILVTable::Entry, 8> vtableEntries;
    vtableEntries.reserve(vtableMethods.size() + 2);

    // For each base method/override pair, emit a vtable thunk or direct
    // reference to the method implementation.
    for (auto method : vtableMethods) {
      SILDeclRef baseRef, derivedRef;
      std::tie(baseRef, derivedRef) = method;

      auto entry = SGM.emitVTableMethod(theClass, derivedRef, baseRef);

      // We might skip emitting entries if the base class is resilient.
      if (entry)
        vtableEntries.push_back(*entry);
    }

    // Add the deallocating destructor to the vtable just for the purpose
    // that it is referenced and cannot be eliminated by dead function removal.
    // In reality, the deallocating destructor is referenced directly from
    // the HeapMetadata for the class.
    {
      auto *dtor = theClass->getDestructor();
      SILDeclRef dtorRef(dtor, SILDeclRef::Kind::Deallocator);
      auto *dtorFn = SGM.getFunction(dtorRef, NotForDefinition);
      vtableEntries.push_back({dtorRef, dtorFn,
                               SILVTable::Entry::Kind::Normal,
                               dtorFn->getLinkage()});
    }

    if (SGM.requiresIVarDestroyer(theClass)) {
      SILDeclRef dtorRef(theClass, SILDeclRef::Kind::IVarDestroyer);
      auto *dtorFn = SGM.getFunction(dtorRef, NotForDefinition);
      vtableEntries.push_back({dtorRef, dtorFn,
                               SILVTable::Entry::Kind::Normal,
                               dtorFn->getLinkage()});
    }

    IsSerialized_t serialized = IsNotSerialized;
    auto classIsPublic = theClass->getEffectiveAccess() >= AccessLevel::Public;
    // Only public, fixed-layout classes should have serialized vtables.
    if (classIsPublic && !theClass->isResilient())
      serialized = IsSerialized;

    // Finally, create the vtable.
    SILVTable::create(SGM.M, theClass, serialized, vtableEntries);
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

  void addPlaceholder(MissingMemberDecl *m) {
    assert(m->getNumberOfVTableEntries() == 0
           && "Should not be emitting class with missing members");
  }
};

} // end anonymous namespace

static void emitTypeMemberGlobalVariable(SILGenModule &SGM,
                                         VarDecl *var) {
  if (var->getDeclContext()->isGenericContext()) {
    assert(var->getDeclContext()->getGenericSignatureOfContext()
              ->areAllParamsConcrete()
           && "generic static vars are not implemented yet");
  }

  if (var->getDeclContext()->getSelfClassDecl()) {
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
  void addMethod(SILDeclRef requirementRef) {
    auto reqAccessor = dyn_cast<AccessorDecl>(requirementRef.getDecl());

    // If it's not an accessor, just look for the witness.
    if (!reqAccessor) {
      if (auto witness = asDerived().getWitness(requirementRef.getDecl())) {
        return addMethodImplementation(requirementRef,
                                       SILDeclRef(witness.getDecl(),
                                                  requirementRef.kind),
                                       witness);
      }

      return asDerived().addMissingMethod(requirementRef);
    }

    // Otherwise, we need to map the storage declaration and then get
    // the appropriate accessor for it.
    auto witness = asDerived().getWitness(reqAccessor->getStorage());
    if (!witness)
      return asDerived().addMissingMethod(requirementRef);

    auto witnessStorage = cast<AbstractStorageDecl>(witness.getDecl());
    auto witnessAccessor =
      witnessStorage->getAccessor(reqAccessor->getAccessorKind());
    if (!witnessAccessor)
      return asDerived().addMissingMethod(requirementRef);

    return addMethodImplementation(requirementRef,
                                   SILDeclRef(witnessAccessor,
                                              SILDeclRef::Kind::Func),
                                   witness);
  }

private:
  void addMethodImplementation(SILDeclRef requirementRef,
                               SILDeclRef witnessRef,
                               Witness witness) {
    // Free function witnesses have an implicit uncurry layer imposed on them by
    // the inserted metatype argument.
    auto isFree =
      isFreeFunctionWitness(requirementRef.getDecl(), witnessRef.getDecl());
    asDerived().addMethodImplementation(requirementRef, witnessRef,
                                        isFree, witness);
  }
};

static IsSerialized_t isConformanceSerialized(RootProtocolConformance *conf) {
  return SILWitnessTable::conformanceIsSerialized(conf)
           ? IsSerialized : IsNotSerialized;
}

/// Emit a witness table for a protocol conformance.
class SILGenConformance : public SILGenWitnessTable<SILGenConformance> {
  using super = SILGenWitnessTable<SILGenConformance>;

public:
  SILGenModule &SGM;
  NormalProtocolConformance *Conformance;
  std::vector<SILWitnessTable::Entry> Entries;
  std::vector<SILWitnessTable::ConditionalConformance> ConditionalConformances;
  SILLinkage Linkage;
  IsSerialized_t Serialized;

  SILGenConformance(SILGenModule &SGM, NormalProtocolConformance *C)
    // We only need to emit witness tables for base NormalProtocolConformances.
    : SGM(SGM), Conformance(C->getRootNormalConformance()),
      Linkage(getLinkageForProtocolConformance(Conformance,
                                               ForDefinition)),
      Serialized(isConformanceSerialized(Conformance))
  {
    auto *proto = Conformance->getProtocol();

    // Not all protocols use witness tables; in this case we just skip
    // all of emit() below completely.
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      Conformance = nullptr;
  }

  SILWitnessTable *emit() {
    // Nothing to do if this wasn't a normal conformance.
    if (!Conformance)
      return nullptr;

    auto *proto = Conformance->getProtocol();
    visitProtocolDecl(proto);

    addConditionalRequirements();

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
        wt->convertToDefinition(Entries, ConditionalConformances, Serialized);

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
    return SILWitnessTable::create(SGM.M, Linkage, Serialized, Conformance,
                                   Entries, ConditionalConformances);
  }

  void addProtocolConformanceDescriptor() {
  }


  void addOutOfLineBaseProtocol(ProtocolDecl *baseProtocol) {
    assert(Lowering::TypeConverter::protocolRequiresWitnessTable(baseProtocol));

    auto conformance = Conformance->getInheritedConformance(baseProtocol);

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

  Witness getWitness(ValueDecl *decl) {
    return Conformance->getWitness(decl, nullptr);
  }

  void addPlaceholder(MissingMemberDecl *placeholder) {
    llvm_unreachable("generating a witness table with placeholders in it");
  }

  void addMissingMethod(SILDeclRef requirement) {
    llvm_unreachable("generating a witness table with placeholders in it");
  }

  void addMethodImplementation(SILDeclRef requirementRef,
                               SILDeclRef witnessRef,
                               IsFreeFunctionWitness_t isFree,
                               Witness witness) {
    // Emit the witness thunk and add it to the table.
    auto witnessLinkage = witnessRef.getLinkage(ForDefinition);
    auto witnessSerialized = Serialized;
    if (witnessSerialized &&
        fixmeWitnessHasLinkageThatNeedsToBePublic(witnessLinkage)) {
      witnessLinkage = SILLinkage::Public;
      witnessSerialized = IsNotSerialized;
    } else {
      // This is the "real" rule; the above case should go away once we
      // figure out what's going on.
      witnessLinkage = (witnessSerialized
                        ? SILLinkage::Shared
                        : SILLinkage::Private);
    }

    SILFunction *witnessFn = SGM.emitProtocolWitness(
        ProtocolConformanceRef(Conformance), witnessLinkage, witnessSerialized,
        requirementRef, witnessRef, isFree, witness);
    Entries.push_back(
                    SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void addAssociatedType(AssociatedType requirement) {
    // Find the substitution info for the witness type.
    auto td = requirement.getAssociation();
    Type witness = Conformance->getTypeWitness(td, /*resolver=*/nullptr);

    // Emit the record for the type itself.
    Entries.push_back(SILWitnessTable::AssociatedTypeWitness{td,
                                                witness->getCanonicalType()});
  }

  void addAssociatedConformance(AssociatedConformance req) {
    auto assocConformance =
      Conformance->getAssociatedConformance(req.getAssociation(),
                                            req.getAssociatedRequirement());

    SGM.useConformance(assocConformance);

    Entries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        req.getAssociation(), req.getAssociatedRequirement(),
        assocConformance});
  }

  void addConditionalRequirements() {
    SILWitnessTable::enumerateWitnessTableConditionalConformances(
        Conformance, [&](unsigned, CanType type, ProtocolDecl *protocol) {
          auto conformance =
              Conformance->getGenericSignature()->lookupConformance(type,
                                                                    protocol);
          assert(conformance &&
                 "unable to find conformance that should be known");

          ConditionalConformances.push_back(
              SILWitnessTable::ConditionalConformance{type, *conformance});

          return /*finished?*/ false;
        });
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

SILFunction *SILGenModule::emitProtocolWitness(
    ProtocolConformanceRef conformance, SILLinkage linkage,
    IsSerialized_t isSerialized, SILDeclRef requirement, SILDeclRef witnessRef,
    IsFreeFunctionWitness_t isFree, Witness witness) {
  auto requirementInfo = Types.getConstantInfo(requirement);

  // Work out the lowered function type of the SIL witness thunk.
  auto reqtOrigTy = cast<GenericFunctionType>(requirementInfo.LoweredType);

  // Mapping from the requirement's generic signature to the witness
  // thunk's generic signature.
  auto reqtSubMap = witness.getRequirementToSyntheticSubs();

  // The generic environment for the witness thunk.
  auto *genericEnv = witness.getSyntheticEnvironment();
  CanGenericSignature genericSig;
  if (genericEnv)
    genericSig = genericEnv->getGenericSignature()->getCanonicalSignature();

  // The type of the witness thunk.
  auto reqtSubstTy = cast<AnyFunctionType>(
    reqtOrigTy->substGenericArgs(reqtSubMap)
      ->getCanonicalType(genericSig));

  // Generic signatures where all parameters are concrete are lowered away
  // at the SILFunctionType level.
  if (genericSig && genericSig->areAllParamsConcrete()) {
    genericSig = nullptr;
    genericEnv = nullptr;
  }

  // Rewrite the conformance in terms of the requirement environment's Self
  // type, which might have a different generic signature than the type
  // itself.
  //
  // For example, if the conforming type is a class and the witness is defined
  // in a protocol extension, the generic signature will have an additional
  // generic parameter representing Self, so the generic parameters of the
  // class will all be shifted down by one.
  if (reqtSubMap) {
    auto requirement = conformance.getRequirement();
    auto self = requirement->getSelfInterfaceType()->getCanonicalType();

    conformance = *reqtSubMap.lookupConformance(self, requirement);
  }

  reqtSubstTy =
    CanAnyFunctionType::get(genericSig,
                            reqtSubstTy->getParams(),
                            reqtSubstTy.getResult(),
                            reqtOrigTy->getExtInfo());

  // Coroutine lowering requires us to provide these substitutions
  // in order to recreate the appropriate yield types for the accessor
  // because they aren't reflected in the accessor's AST type.
  // But this is expensive, so we only do it for coroutine lowering.
  // When they're part of the AST function type, we can remove this
  // parameter completely.
  Optional<SubstitutionMap> witnessSubsForTypeLowering;
  if (auto accessor = dyn_cast<AccessorDecl>(requirement.getDecl())) {
    if (accessor->isCoroutine()) {
      witnessSubsForTypeLowering =
        witness.getSubstitutions().mapReplacementTypesOutOfContext();
    }
  }

  // Lower the witness thunk type with the requirement's abstraction level.
  auto witnessSILFnType = getNativeSILFunctionType(
      M, AbstractionPattern(reqtOrigTy), reqtSubstTy,
      requirement, witnessRef, witnessSubsForTypeLowering, conformance);

  // Mangle the name of the witness thunk.
  Mangle::ASTMangler NewMangler;
  auto manglingConformance =
      conformance.isConcrete() ? conformance.getConcrete() : nullptr;
  std::string nameBuffer =
      NewMangler.mangleWitnessThunk(manglingConformance, requirement.getDecl());

  // If the thunked-to function is set to be always inlined, do the
  // same with the witness, on the theory that the user wants all
  // calls removed if possible, e.g. when we're able to devirtualize
  // the witness method call. Otherwise, use the default inlining
  // setting on the theory that forcing inlining off should only
  // effect the user's function, not otherwise invisible thunks.
  Inline_t InlineStrategy = InlineDefault;
  if (witnessRef.isAlwaysInline())
    InlineStrategy = AlwaysInline;

  SILGenFunctionBuilder builder(*this);
  auto *f = builder.createFunction(
      linkage, nameBuffer, witnessSILFnType, genericEnv,
      SILLocation(witnessRef.getDecl()), IsNotBare, IsTransparent, isSerialized,
      IsNotDynamic, ProfileCounter(), IsThunk, SubclassScope::NotApplicable,
      InlineStrategy);

  f->setDebugScope(new (M)
                   SILDebugScope(RegularLocation(witnessRef.getDecl()), f));

  PrettyStackTraceSILFunction trace("generating protocol witness thunk", f);

  // Create the witness.
  SILGenFunction SGF(*this, *f, SwiftModule);

  // Substitutions mapping the generic parameters of the witness to
  // archetypes of the witness thunk generic environment.
  auto witnessSubs = witness.getSubstitutions();

  SGF.emitProtocolWitness(AbstractionPattern(reqtOrigTy), reqtSubstTy,
                          requirement, reqtSubMap, witnessRef,
                          witnessSubs, isFree, /*isSelfConformance*/ false);

  return f;
}

namespace {

static SILFunction *emitSelfConformanceWitness(SILGenModule &SGM,
                                           SelfProtocolConformance *conformance,
                                               SILLinkage linkage,
                                               SILDeclRef requirement) {
  auto requirementInfo = SGM.Types.getConstantInfo(requirement);

  // Work out the lowered function type of the SIL witness thunk.
  auto reqtOrigTy = cast<GenericFunctionType>(requirementInfo.LoweredType);

  // The transformations we do here don't work for generic requirements.
  GenericEnvironment *genericEnv = nullptr;

  // A mapping from the requirement's generic signature to the type parameters
  // of the witness thunk (which is non-generic).
  auto protocol = conformance->getProtocol();
  auto protocolType = protocol->getDeclaredInterfaceType();
  auto reqtSubs = SubstitutionMap::getProtocolSubstitutions(protocol,
                                          protocolType,
                                          ProtocolConformanceRef(protocol));

  // Open the protocol type.
  auto openedType = ArchetypeType::getOpened(protocolType);

  // Form the substitutions for calling the witness.
  auto witnessSubs = SubstitutionMap::getProtocolSubstitutions(protocol,
                                          openedType,
                                          ProtocolConformanceRef(protocol));

  // Substitute to get the formal substituted type of the thunk.
  auto reqtSubstTy =
    cast<AnyFunctionType>(reqtOrigTy.subst(reqtSubs)->getCanonicalType());

  // Substitute into the requirement type to get the type of the thunk.
  auto witnessSILFnType =
    requirementInfo.SILFnType->substGenericArgs(SGM.M, reqtSubs);

  // Mangle the name of the witness thunk.
  std::string name = [&] {
    Mangle::ASTMangler mangler;
    return mangler.mangleWitnessThunk(conformance, requirement.getDecl());
  }();

  SILGenFunctionBuilder builder(SGM);
  auto *f = builder.createFunction(
      linkage, name, witnessSILFnType, genericEnv,
      SILLocation(requirement.getDecl()), IsNotBare, IsTransparent,
      IsSerialized, IsNotDynamic, ProfileCounter(), IsThunk,
      SubclassScope::NotApplicable, InlineDefault);

  f->setDebugScope(new (SGM.M)
                   SILDebugScope(RegularLocation(requirement.getDecl()), f));

  PrettyStackTraceSILFunction trace("generating protocol witness thunk", f);

  // Create the witness.
  SILGenFunction SGF(SGM, *f, SGM.SwiftModule);

  auto isFree = isFreeFunctionWitness(requirement.getDecl(),
                                      requirement.getDecl());

  SGF.emitProtocolWitness(AbstractionPattern(reqtOrigTy), reqtSubstTy,
                          requirement, reqtSubs, requirement,
                          witnessSubs, isFree, /*isSelfConformance*/ true);

  return f;
}

/// Emit a witness table for a self-conformance.
class SILGenSelfConformanceWitnessTable
       : public SILWitnessVisitor<SILGenSelfConformanceWitnessTable> {
  using super = SILWitnessVisitor<SILGenSelfConformanceWitnessTable>;

  SILGenModule &SGM;
  SelfProtocolConformance *conformance;
  SILLinkage linkage;
  IsSerialized_t serialized;

  SmallVector<SILWitnessTable::Entry, 8> entries;
public:
  SILGenSelfConformanceWitnessTable(SILGenModule &SGM,
                                    SelfProtocolConformance *conformance)
    : SGM(SGM), conformance(conformance),
      linkage(getLinkageForProtocolConformance(conformance, ForDefinition)),
      serialized(isConformanceSerialized(conformance)) {
  }

  void emit() {
    // Add entries for all the requirements.
    visitProtocolDecl(conformance->getProtocol());

    // Create the witness table.
    (void) SILWitnessTable::create(SGM.M, linkage, serialized, conformance,
                                   entries, /*conditional*/ {});
  }

  void addProtocolConformanceDescriptor() {}

  void addOutOfLineBaseProtocol(ProtocolDecl *protocol) {
    // This is an unnecessary restriction that's just not necessary for Error.
    llvm_unreachable("base protocols not supported in self-conformance");
  }

  // These are real semantic restrictions.
  void addAssociatedConformance(AssociatedConformance conformance) {
    llvm_unreachable("associated conformances not supported in self-conformance");
  }
  void addAssociatedType(AssociatedType type) {
    llvm_unreachable("associated types not supported in self-conformance");
  }
  void addPlaceholder(MissingMemberDecl *placeholder) {
    llvm_unreachable("placeholders not supported in self-conformance");
  }

  void addMethod(SILDeclRef requirement) {
    auto witness = emitSelfConformanceWitness(SGM, conformance, linkage,
                                              requirement);
    entries.push_back(SILWitnessTable::MethodWitness{requirement, witness});
  }
};
}

void SILGenModule::emitSelfConformanceWitnessTable(ProtocolDecl *protocol) {
  auto conformance = getASTContext().getSelfConformance(protocol);
  SILGenSelfConformanceWitnessTable(*this, conformance).emit();
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

  void addProtocolConformanceDescriptor() { }

  void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
    addMissingDefault();
  }

  void addMissingMethod(SILDeclRef ref) {
    addMissingDefault();
  }

  void addPlaceholder(MissingMemberDecl *placeholder) {
    llvm_unreachable("generating a witness table with placeholders in it");
  }

  Witness getWitness(ValueDecl *decl) {
    return Proto->getDefaultWitness(decl);
  }

  void addMethodImplementation(SILDeclRef requirementRef,
                               SILDeclRef witnessRef,
                               IsFreeFunctionWitness_t isFree,
                               Witness witness) {
    SILFunction *witnessFn = SGM.emitProtocolWitness(
        ProtocolConformanceRef(Proto), SILLinkage::Private, IsNotSerialized,
        requirementRef, witnessRef, isFree, witness);
    auto entry = SILWitnessTable::MethodWitness{requirementRef, witnessFn};
    DefaultWitnesses.push_back(entry);
  }

  void addAssociatedType(AssociatedType req) {
    Type witness = Proto->getDefaultTypeWitness(req.getAssociation());
    if (!witness)
      return addMissingDefault();

    Type witnessInContext = Proto->mapTypeIntoContext(witness);
    auto entry = SILWitnessTable::AssociatedTypeWitness{
                                          req.getAssociation(),
                                          witnessInContext->getCanonicalType()};
    DefaultWitnesses.push_back(entry);
  }

  void addAssociatedConformance(const AssociatedConformance &req) {
    auto witness =
        Proto->getDefaultAssociatedConformanceWitness(
          req.getAssociation(),
          req.getAssociatedRequirement());
    if (!witness)
      return addMissingDefault();

    auto entry =
        SILWitnessTable::AssociatedTypeProtocolWitness{
          req.getAssociation(),
          req.getAssociatedRequirement(),
          *witness};
    DefaultWitnesses.push_back(entry);
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

    // Build a default witness table if this is a protocol that needs one.
    if (auto protocol = dyn_cast<ProtocolDecl>(theType)) {
      if (!protocol->isObjC() && protocol->isResilient()) {
        auto *SF = protocol->getParentSourceFile();
        if (!SF || SF->Kind != SourceFileKind::Interface)
          SGM.emitDefaultWitnessTable(protocol);
      }
      if (protocol->requiresSelfConformanceWitnessTable()) {
        SGM.emitSelfConformanceWitnessTable(protocol);
      }
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
  void visitMissingMemberDecl(MissingMemberDecl *) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    SGM.emitFunction(fd);
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCMethodEntryPoint(fd) &&
        !isa<ProtocolDecl>(fd->getDeclContext()))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    SGM.emitConstructor(cd);

    if (SGM.requiresObjCMethodEntryPoint(cd) &&
        !isa<ProtocolDecl>(cd->getDeclContext()))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    assert(isa<ClassDecl>(theType) && "destructor in a non-class type");
    SGM.emitDestructor(cast<ClassDecl>(theType), dd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ued) {}

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers.
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i) {
      if (pd->getNonLazyInit(i)) {
        if (pd->isStatic())
          SGM.emitGlobalInitialization(pd, i);
        else
          SGM.emitStoredPropertyInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
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
    
    SGM.tryEmitPropertyDescriptor(asd);
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

    if (!isa<ProtocolDecl>(e->getExtendedNominal())) {
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
  void visitMissingMemberDecl(MissingMemberDecl *) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    SGM.emitFunction(fd);
    if (SGM.requiresObjCMethodEntryPoint(fd))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
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
      if (pd->getNonLazyInit(i)) {
        assert(pd->isStatic() && "stored property in extension?!");
        SGM.emitGlobalInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
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
    
    SGM.tryEmitPropertyDescriptor(vd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}
