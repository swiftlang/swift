#define DEBUG_TYPE "opaque-archetype-specializer"

#include "swift/AST/Types.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"


using namespace swift;

/// A function object that can be used as a \c TypeSubstitutionFn and
/// \c LookupConformanceFn for \c Type::subst style APIs to map opaque
/// archetypes with underlying types visible at a given resilience expansion
/// to their underlying types.
class ReplaceOpaqueTypesWithUnderlyingTypes {
public:
  SILFunction *context;
  ReplaceOpaqueTypesWithUnderlyingTypes(
      SILFunction *context)
      : context(context) {}

  /// TypeSubstitutionFn
  Type operator()(SubstitutableType *maybeOpaqueType) const;

  /// LookupConformanceFn
  Optional<ProtocolConformanceRef> operator()(CanType maybeOpaqueType,
                                              Type replacementType,
                                              ProtocolDecl *protocol) const;

  bool shouldPerformSubstitution(OpaqueTypeDecl *opaque) const;

  static bool shouldPerformSubstitution(OpaqueTypeDecl *opaque,
                                        SILFunction *context);
};

static Type substOpaqueTypesWithUnderlyingTypes(
    Type ty, SILFunction *context) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(context);
  return ty.subst(replacer, replacer, SubstFlags::SubstituteOpaqueArchetypes);
}

static SubstitutionMap
substOpaqueTypesWithUnderlyingTypes(SubstitutionMap map, SILFunction *context) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(context);
  return map.subst(replacer, replacer, SubstFlags::SubstituteOpaqueArchetypes);
}

static ProtocolConformanceRef
substOpaqueTypesWithUnderlyingTypes(ProtocolConformanceRef ref, Type origType,
                                    SILFunction *context) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(context);
  return ref.subst(origType, replacer, replacer,
                   SubstFlags::SubstituteOpaqueArchetypes);
}

static Optional<std::pair<ArchetypeType *, OpaqueTypeArchetypeType*>>
getArchetypeAndRootOpaqueArchetype(Type maybeOpaqueType) {
  auto archetype = dyn_cast<ArchetypeType>(maybeOpaqueType.getPointer());
  if (!archetype)
    return None;
  auto opaqueRoot = dyn_cast<OpaqueTypeArchetypeType>(archetype->getRoot());
  if (!opaqueRoot)
    return None;

  return std::make_pair(archetype, opaqueRoot);
}
bool ReplaceOpaqueTypesWithUnderlyingTypes::shouldPerformSubstitution(
    OpaqueTypeDecl *opaque) const {
  return shouldPerformSubstitution(opaque, context);
}

bool ReplaceOpaqueTypesWithUnderlyingTypes::shouldPerformSubstitution(
    OpaqueTypeDecl *opaque, SILFunction *context) {
  auto namingDecl = opaque->getNamingDecl();

  // Allow replacement of opaque result types of inlineable function regardless
  // of resilience and in which context.
  if (namingDecl->getAttrs().hasAttribute<InlinableAttr>()) {
    return true;
  }
  // Allow replacement of opaque result types in the context of maximal
  // resilient expansion if the context's and the opaque type's module are the
  // same.
  auto expansion = context->getResilienceExpansion();
  auto module = namingDecl->getModuleContext();
  if (expansion == ResilienceExpansion::Maximal &&
      module == context->getModule().getSwiftModule())
    return true;

  // Allow general replacement from non resilient modules. Otherwise, disallow.
  return !module->isResilient();
}

Type ReplaceOpaqueTypesWithUnderlyingTypes::
operator()(SubstitutableType *maybeOpaqueType) const {
  auto archetypeAndRoot = getArchetypeAndRootOpaqueArchetype(maybeOpaqueType);
  if (!archetypeAndRoot)
    return maybeOpaqueType;

  auto archetype = archetypeAndRoot->first;
  auto opaqueRoot = archetypeAndRoot->second;

  if (!shouldPerformSubstitution(opaqueRoot->getDecl())) {
    return maybeOpaqueType;
  }

  auto subs = opaqueRoot->getDecl()->getUnderlyingTypeSubstitutions();
  // TODO: Check the resilience expansion, and handle opaque types with
  // unknown underlying types. For now, all opaque types are always
  // fragile.
  assert(subs.hasValue() && "resilient opaque types not yet supported");

  // Apply the underlying type substitutions to the interface type of the
  // archetype in question. This will map the inner generic signature of the
  // opaque type to its outer signature.
  auto partialSubstTy = archetype->getInterfaceType().subst(*subs);
  // Then apply the substitutions from the root opaque archetype, to specialize
  // for its type arguments.
  auto substTy = partialSubstTy.subst(opaqueRoot->getSubstitutions());

  // If the type still contains opaque types, recur.
  if (substTy->hasOpaqueArchetype()) {
    return substOpaqueTypesWithUnderlyingTypes(substTy,
                                               context);
  }
  return substTy;
}

Optional<ProtocolConformanceRef> ReplaceOpaqueTypesWithUnderlyingTypes::
operator()(CanType maybeOpaqueType, Type replacementType,
           ProtocolDecl *protocol) const {
  auto abstractRef = ProtocolConformanceRef(protocol);

  auto archetypeAndRoot = getArchetypeAndRootOpaqueArchetype(maybeOpaqueType);
  if (!archetypeAndRoot) {
    assert(maybeOpaqueType->isTypeParameter() ||
           maybeOpaqueType->is<ArchetypeType>());
    return abstractRef;
  }

  auto archetype = archetypeAndRoot->first;
  auto opaqueRoot = archetypeAndRoot->second;

  if (!shouldPerformSubstitution(opaqueRoot->getDecl())) {
    return abstractRef;
  }

  auto subs = opaqueRoot->getDecl()->getUnderlyingTypeSubstitutions();
  assert(subs.hasValue());

  // Apply the underlying type substitutions to the interface type of the
  // archetype in question. This will map the inner generic signature of the
  // opaque type to its outer signature.
  auto partialSubstTy = archetype->getInterfaceType().subst(*subs);
  auto partialSubstRef =
      abstractRef.subst(archetype->getInterfaceType(), *subs);

  // Then apply the substitutions from the root opaque archetype, to specialize
  // for its type arguments.
  auto substTy = partialSubstTy.subst(opaqueRoot->getSubstitutions());
  auto substRef =
      partialSubstRef.subst(partialSubstTy, opaqueRoot->getSubstitutions());

  // If the type still contains opaque types, recur.
  if (substTy->hasOpaqueArchetype()) {
    return substOpaqueTypesWithUnderlyingTypes(substRef, substTy, context);
  }
  return substRef;
}

namespace {
class OpaqueSpecializerCloner
    : public SILCloner<OpaqueSpecializerCloner> {

  using SuperTy = SILCloner<OpaqueSpecializerCloner>;

  SILBasicBlock *entryBlock;
  SILBasicBlock *cloneFromBlock;

  /// Cache for substituted types.
  llvm::DenseMap<SILType, SILType> TypeCache;

  SILFunction &Original;

public:
  friend class SILCloner<OpaqueSpecializerCloner>;
  friend class SILCloner<OpaqueSpecializerCloner>;
  friend class SILInstructionVisitor<OpaqueSpecializerCloner>;

  OpaqueSpecializerCloner(SILFunction &fun) : SuperTy(fun), Original(fun) {
    entryBlock = fun.getEntryBlock();
    cloneFromBlock = entryBlock->split(entryBlock->begin());
  }

  void clone();

protected:
  void insertOpaqueToConcreteAddressCasts(SILInstruction *orig,
                                          SILInstruction *cloned);

  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    SILCloner<OpaqueSpecializerCloner>::postProcess(orig, cloned);
    insertOpaqueToConcreteAddressCasts(orig, cloned);
  }

  void visitTerminator(SILBasicBlock *BB) {
    visit(BB->getTerminator());
  }

  void visitReturnInst(ReturnInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    auto origResult = Inst->getOperand();
    auto clonedResult = getOpValue(Inst->getOperand());
    if (clonedResult->getType().getASTType() !=
        origResult->getType().getASTType())
      clonedResult = getBuilder().createUncheckedRefCast(
          RegularLocation::getAutoGeneratedLocation(), clonedResult,
          origResult->getType());
    recordClonedInstruction(
        Inst,
        getBuilder().createReturn(getOpLocation(Inst->getLoc()), clonedResult));
  }

  /// Projections should not change the type if the type is not specialized.
  void visitStructElementAddrInst(StructElementAddrInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    auto opd = getOpValue(Inst->getOperand());
    recordClonedInstruction(
        Inst, getBuilder().createStructElementAddr(
                  getOpLocation(Inst->getLoc()), opd, Inst->getField()));
  }

  /// Projections should not change the type if the type is not specialized.
  void visitStructExtractInst(StructExtractInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    auto opd = getOpValue(Inst->getOperand());
    recordClonedInstruction(
        Inst, getBuilder().createStructExtract(getOpLocation(Inst->getLoc()),
                                               opd, Inst->getField()));
  }
  /// Projections should not change the type if the type is not specialized.
  void visitTupleElementAddrInst(TupleElementAddrInst *Inst) {
    auto opd = getOpValue(Inst->getOperand());
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    recordClonedInstruction(Inst, getBuilder().createTupleElementAddr(
                                      getOpLocation(Inst->getLoc()), opd,
                                      Inst->getFieldNo()));
  }
  /// Projections should not change the type if the type is not specialized.
  void visitTupleExtractInst(TupleExtractInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    recordClonedInstruction(
        Inst, getBuilder().createTupleExtract(getOpLocation(Inst->getLoc()),
                                              getOpValue(Inst->getOperand()),
                                              Inst->getFieldNo()));
  }
  /// Projections should not change the type if the type is not specialized.
  void visitRefElementAddrInst(RefElementAddrInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    recordClonedInstruction(
        Inst, getBuilder().createRefElementAddr(
                  getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand()),
                  Inst->getField()));
  }

  /// Projections should not change the type if the type is not specialized.
  void visitRefTailAddrInst(RefTailAddrInst *Inst) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    recordClonedInstruction(
        Inst, getBuilder().createRefTailAddr(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             Inst->getType()));
  }

  void visitYieldInst(YieldInst *Inst) {
    auto OrigValues = Inst->getYieldedValues();
    auto Values = getOpValueArray<8>(Inst->getYieldedValues());
    auto ResumeBB = getOpBasicBlock(Inst->getResumeBB());
    auto UnwindBB = getOpBasicBlock(Inst->getUnwindBB());
    for (auto idx : indices(Values)) {
      if (OrigValues[idx]->getType().getASTType() !=
          Values[idx]->getType().getASTType()) {
        if (!Values[idx]->getType().isAddress()) {
          Values[idx] = getBuilder().createUncheckedRefCast(
              RegularLocation::getAutoGeneratedLocation(), Values[idx],
              OrigValues[idx]->getType());
        } else {
          Values[idx] = getBuilder().createUncheckedAddrCast(
              RegularLocation::getAutoGeneratedLocation(), Values[idx],
              OrigValues[idx]->getType());
        }
      }
    }

    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    recordClonedInstruction(
        Inst, getBuilder().createYield(getOpLocation(Inst->getLoc()), Values,
                                       ResumeBB, UnwindBB));
  }

  void visitCopyAddrInst(CopyAddrInst *Inst) {
    auto src = getOpValue(Inst->getSrc());
    auto dst = getOpValue(Inst->getDest());
    auto srcType = src->getType();
    auto destType = dst->getType();
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    // If the types mismatch cast the operands to the non opaque archetype.
    if (destType.getASTType() != srcType.getASTType()) {
      if (srcType.getASTType()->hasOpaqueArchetype()) {
        src = getBuilder().createUncheckedAddrCast(
            getOpLocation(Inst->getLoc()), src, destType);
      } else if (destType.getASTType()->hasOpaqueArchetype()) {
        dst = getBuilder().createUncheckedAddrCast(
            getOpLocation(Inst->getLoc()), dst, srcType);
      }
    }
    recordClonedInstruction(
        Inst, getBuilder().createCopyAddr(getOpLocation(Inst->getLoc()), src,
                                          dst, Inst->isTakeOfSrc(),
                                          Inst->isInitializationOfDest()));
  }

  void visitStoreInst(StoreInst *Inst) {
    auto src = getOpValue(Inst->getSrc());
    auto dst = getOpValue(Inst->getDest());
    auto srcType = src->getType();
    auto destType = dst->getType();
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
    // If the types mismatch cast the operands to the non opaque archetype.
    if (destType.getASTType() != srcType.getASTType()) {
      if (srcType.getASTType()->hasOpaqueArchetype()) {
        assert(!srcType.isAddress());
        src = getBuilder().createUncheckedRefCast(
            getOpLocation(Inst->getLoc()), src, destType.getObjectType());
      } else if (destType.getASTType()->hasOpaqueArchetype()) {
        dst = getBuilder().createUncheckedAddrCast(
            getOpLocation(Inst->getLoc()), dst, srcType.getAddressType());
      }
    }

    if (!getBuilder().hasOwnership()) {
      switch (Inst->getOwnershipQualifier()) {
      case StoreOwnershipQualifier::Assign: {
        auto *li = getBuilder().createLoad(getOpLocation(Inst->getLoc()), dst,
                                           LoadOwnershipQualifier::Unqualified);
        auto *si = getBuilder().createStore(
            getOpLocation(Inst->getLoc()), src, getOpValue(Inst->getDest()),
            StoreOwnershipQualifier::Unqualified);
        getBuilder().emitDestroyValueOperation(getOpLocation(Inst->getLoc()),
                                               li);
        return recordClonedInstruction(Inst, si);
      }
      case StoreOwnershipQualifier::Init:
      case StoreOwnershipQualifier::Trivial:
      case StoreOwnershipQualifier::Unqualified:
        break;
      }

      return recordClonedInstruction(
          Inst,
          getBuilder().createStore(getOpLocation(Inst->getLoc()), src, dst,
                                   StoreOwnershipQualifier::Unqualified));
    }

    recordClonedInstruction(
        Inst, getBuilder().createStore(getOpLocation(Inst->getLoc()), src, dst,
                                       Inst->getOwnershipQualifier()));
  }

protected:

  SILType remapType(SILType Ty) {
    SILType &Sty = TypeCache[Ty];
    if (Sty)
      return Sty;

   // Apply the opaque types substitution.
    ReplaceOpaqueTypesWithUnderlyingTypes replacer(&Original);
    Sty = Ty.subst(Original.getModule(), replacer, replacer,
                   CanGenericSignature(), true);
    return Sty;
  }

  CanType remapASTType(CanType ty) {
    // Apply the opaque types substitution.
    return substOpaqueTypesWithUnderlyingTypes(ty, &Original)
        ->getCanonicalType();
  }

  ProtocolConformanceRef remapConformance(Type type,
                                          ProtocolConformanceRef conf) {
    // Apply the opaque types substitution.
    ReplaceOpaqueTypesWithUnderlyingTypes replacer(&Original);
    return conf.subst(type, replacer, replacer,
                      SubstFlags::SubstituteOpaqueArchetypes);
  }

  SubstitutionMap remapSubstitutionMap(SubstitutionMap Subs) {
    // Apply the opaque types substitution.
    return substOpaqueTypesWithUnderlyingTypes(Subs, &Original);
  }
};
} // namespace

void OpaqueSpecializerCloner::clone() {
  for (auto arg: entryBlock->getArguments())
    recordFoldedValue(arg, arg);
  cloneReachableBlocks(cloneFromBlock, {}, entryBlock,
                       true /*havePrepopulatedFunctionArgs*/);
  getBuilder().setInsertionPoint(entryBlock);
  getBuilder().createBranch(RegularLocation::getAutoGeneratedLocation(),
                            getOpBasicBlock(cloneFromBlock));
}

/// Update address uses of the opaque type archetype with the concrete type.
/// This is neccessary for apply instructions.
void OpaqueSpecializerCloner::insertOpaqueToConcreteAddressCasts(
    SILInstruction *orig, SILInstruction *cloned) {

  // Replace apply operands.
  if (auto apply = ApplySite::isa(cloned)) {
    SavedInsertionPointRAII restore(getBuilder());
    getBuilder().setInsertionPoint(apply.getInstruction());
    auto substConv = apply.getSubstCalleeConv();
    unsigned idx = 0;
    for (auto &opd : apply.getArgumentOperands()) {
      auto argConv = apply.getArgumentConvention(opd);
      auto argIdx = apply.getCalleeArgIndex(opd);
      auto argType = substConv.getSILArgumentType(argIdx);
      if (argType.getASTType() != opd.get()->getType().getASTType()) {
        if (argConv.isIndirectConvention()) {
          auto cast = getBuilder().createUncheckedAddrCast(apply.getLoc(),
                                                           opd.get(), argType);
          opd.set(cast);
        } else {
          auto cast = getBuilder().createUncheckedRefCast(apply.getLoc(),
                                                          opd.get(), argType);
          opd.set(cast);
        }
      }
      ++idx;
    }
    return;
  }
}

namespace {
class OpaqueArchetypeSpecializer : public SILFunctionTransform {
  void run() override {
    auto *context = getFunction();

    auto opaqueArchetypeWouldChange = [=](CanType ty) -> bool {
      if (!ty->hasOpaqueArchetype())
        return false;

      return ty.findIf([=](Type type) -> bool {
        if (auto opaqueTy = type->getAs<OpaqueTypeArchetypeType>()) {
          auto opaque = opaqueTy->getDecl();
          return ReplaceOpaqueTypesWithUnderlyingTypes::
              shouldPerformSubstitution(opaque, context);
        }
        return false;
      });
    };

    // Look for opaque type archetypes.
    bool foundOpaqueArchetype = false;
    for (auto &BB : *getFunction()) {
      for (auto &inst : BB) {
        auto hasOpaqueOperand = [&] (SILInstruction &inst) -> bool {
          // Check the operands for opaque types.
          for (auto &opd : inst.getAllOperands())
            if (opaqueArchetypeWouldChange(opd.get()->getType().getASTType()))
              return true;
          return false;
        };
        if ((foundOpaqueArchetype = hasOpaqueOperand(inst)))
          break;
        auto hasOpaqueResult = [&](SILInstruction &inst) -> bool {
          // Check the results for opaque types.
          for (const auto &res : inst.getResults())
            if (opaqueArchetypeWouldChange(res->getType().getASTType()))
              return true;
          return false;
        };
        if ((foundOpaqueArchetype = hasOpaqueResult(inst)))
          break;
      }
      if (foundOpaqueArchetype)
        break;
    }

    if (foundOpaqueArchetype) {
      OpaqueSpecializerCloner s(*getFunction());
      s.clone();
      removeUnreachableBlocks(*getFunction());
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }
  }
};
} // end anonymous namespace

SILTransform *swift::createOpaqueArchetypeSpecializer() {
  return new OpaqueArchetypeSpecializer();
}
