//===--- SILCloner.h - Defines the SILCloner class --------------*- C++ -*-===//
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
// This file defines the SILCloner class, used for cloning SIL instructions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILCLONER_H
#define SWIFT_SIL_SILCLONER_H

#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILVisitor.h"

namespace swift {

/// SILCloner - Abstract SIL visitor which knows how to clone instructions and
/// whose behavior can be customized by subclasses via the CRTP. This is meant
/// to be subclassed to implement inlining, function specialization, and other
/// operations requiring cloning (while possibly modifying, at the same time)
/// instruction sequences.
///
/// By default, this visitor will not do anything useful when called on a
/// basic block, or function; subclasses that want to handle those should
/// implement the appropriate visit functions and/or provide other entry points.
template<typename ImplClass>
class SILCloner : protected SILVisitor<ImplClass> {
  friend class SILVisitor<ImplClass, SILValue>;

public:
  using SILVisitor<ImplClass>::asImpl;

  explicit SILCloner(SILFunction &F,
                     SILOpenedArchetypesTracker &OpenedArchetypesTracker)
      : Builder(F), InsertBeforeBB(nullptr),
        OpenedArchetypesTracker(OpenedArchetypesTracker) {
    Builder.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  }

  explicit SILCloner(SILFunction &F)
      : Builder(F), InsertBeforeBB(nullptr),
        OpenedArchetypesTracker(F) {
    Builder.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  }

  /// Clients of SILCloner who want to know about any newly created
  /// instructions can install a SmallVector into the builder to collect them.
  void setTrackingList(SmallVectorImpl<SILInstruction*> *II) {
    getBuilder().setTrackingList(II);
  }
  
  SmallVectorImpl<SILInstruction*> *getTrackingList() {
    return getBuilder().getTrackingList();
  }

  SILBuilder &getBuilder() { return Builder; }

protected:
  void beforeVisit(ValueBase *V) {
    if (auto I = dyn_cast<SILInstruction>(V)) {
      // Update the set of available opened archetypes with the opened
      // archetypes used by the current instruction.
     doPreProcess(I);
    }
  }

#define VALUE(CLASS, PARENT) \
  void visit##CLASS(CLASS *I) {                                       \
    llvm_unreachable("SILCloner visiting non-instruction?");          \
  }
#define INST(CLASS, PARENT, TEXTUALNAME, MEMBEHAVIOR, RELEASINGBEHAVIOR)       \
  void visit##CLASS(CLASS *I);
#include "swift/SIL/SILNodes.def"

  void visitSILBasicBlock(SILBasicBlock* BB);

  void visitSILFunction(SILFunction *F);

  // Derived classes of SILCloner using the CRTP can implement the following
  // functions to customize behavior; the remap functions are called before
  // cloning to modify constructor arguments and the post process function is
  // called afterwards on the result.
  SILLocation remapLocation(SILLocation Loc) { return Loc; }
  const SILDebugScope *remapScope(const SILDebugScope *DS) { return DS; }
  SILType remapType(SILType Ty) { return Ty; }
  CanType remapASTType(CanType Ty) { return Ty; }
  ProtocolConformanceRef remapConformance(Type Ty, ProtocolConformanceRef C){
    return C;
  }
  SILValue remapValue(SILValue Value);
  SILFunction *remapFunction(SILFunction *Func) { return Func; }
  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB);
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned);

  SILLocation getOpLocation(SILLocation Loc) {
    return asImpl().remapLocation(Loc);
  }
  const SILDebugScope *getOpScope(const SILDebugScope *DS) {
    return asImpl().remapScope(DS);
  }
  SmallVector<Substitution, 4> getOpSubstitutions(SubstitutionList Subs) {
    SmallVector<Substitution, 4> NewSubs;
    for (auto Sub : Subs) {
      NewSubs.push_back(getOpSubstitution(Sub));
    }
    return NewSubs;
  }
  
  SILType getTypeInClonedContext(SILType Ty) {
    // Substitute opened existential types, if we have any.
    return SILType::getPrimitiveObjectType(
      getASTTypeInClonedContext(Ty.getSwiftRValueType()))
      .copyCategory(Ty);
  }
  SILType getOpType(SILType Ty) {
    Ty = getTypeInClonedContext(Ty);
    return asImpl().remapType(Ty);
  }

  CanType getASTTypeInClonedContext(Type ty) {
    // Do not substitute opened existential types, if we do not have any.
    if (!ty->hasOpenedExistential())
      return ty->getCanonicalType();
    // Do not substitute opened existential types, if it is not required.
    // This is often the case when cloning basic blocks inside the same
    // function.
    if (OpenedExistentialSubs.empty())
      return ty->getCanonicalType();

    return ty.transform(
      [&](Type t) -> Type {
        if (t->isOpenedExistential()) {
          auto found = OpenedExistentialSubs.find(
            t->castTo<ArchetypeType>());
          // If an opened existential is supposed to be
          // remapped, it is guaranteed by construction
          // to be in the OpenedExistentialSubs, because
          // a cloner always processes definitions of
          // opened existentials before their uses and
          // adds found opened existentials definitions
          // to the map.
          if (found != OpenedExistentialSubs.end())
            return found->second;
          return t;
        }
        return t;
      })->getCanonicalType();
  }

  CanType getOpASTType(CanType ty) {
    ty = getASTTypeInClonedContext(ty);
    return asImpl().remapASTType(ty);
  }

  ProtocolConformanceRef getOpConformance(Type ty,
                                          ProtocolConformanceRef conformance) {
    auto newConformance =
      conformance.subst(ty,
                        [&](SubstitutableType *t) -> Type {
                          if (t->isOpenedExistential()) {
                            auto found = OpenedExistentialSubs.find(
                              t->castTo<ArchetypeType>());
                            if (found != OpenedExistentialSubs.end())
                              return found->second;
                            return t;
                          }
                          return t;
                        },
                        MakeAbstractConformanceForGenericType());
    return asImpl().remapConformance(getASTTypeInClonedContext(ty),
                                     newConformance);
  }

  ArrayRef<ProtocolConformanceRef>
  getOpConformances(Type ty,
                    ArrayRef<ProtocolConformanceRef> conformances) {
    SmallVector<ProtocolConformanceRef, 4> newConformances;
    for (auto conformance : conformances)
      newConformances.push_back(getOpConformance(ty, conformance));
    return ty->getASTContext().AllocateCopy(newConformances);
  }
  
  Substitution getOpSubstitution(Substitution sub) {
    CanType newReplacement =
      getOpASTType(sub.getReplacement()->getCanonicalType());
    auto conformances = getOpConformances(sub.getReplacement(),
                                          sub.getConformances());
    return Substitution(newReplacement, conformances);
  }

  SILValue getOpValue(SILValue Value) {
    return asImpl().remapValue(Value);
  }
  template <size_t N, typename ArrayRefType>
  SmallVector<SILValue, N> getOpValueArray(ArrayRefType Values) {
    SmallVector<SILValue, N> Ret(Values.size());
    for (unsigned i = 0, e = Values.size(); i != e; ++i)
      Ret[i] = asImpl().remapValue(Values[i]);
    return Ret;
  }
  SILFunction *getOpFunction(SILFunction *Func) {
    return asImpl().remapFunction(Func);
  }
  SILBasicBlock *getOpBasicBlock(SILBasicBlock *BB) {
    return asImpl().remapBasicBlock(BB);
  }
  void addBlockWithUnreachable(SILBasicBlock *BB) {
    BlocksWithUnreachables.insert(BB);
  }

  void cleanUp(SILFunction *F);

public:
  void doPreProcess(SILInstruction *Orig) {
    // Extend the set of available opened archetypes by the opened archetypes
    // used by the instruction being cloned.
    auto TypeDependentOperands = Orig->getTypeDependentOperands();
    Builder.getOpenedArchetypes().addOpenedArchetypeOperands(
        TypeDependentOperands);
  }

  void doPostProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    asImpl().postProcess(Orig, Cloned);
    assert((Orig->getDebugScope() ? Cloned->getDebugScope()!=nullptr : true) &&
           "cloned instruction dropped debug scope");
  }

  // Register a re-mapping for opened existentials.
  void registerOpenedExistentialRemapping(ArchetypeType *From, ArchetypeType *To) {
    auto result =
      OpenedExistentialSubs.insert(std::make_pair(CanArchetypeType(From),
                                                  CanType(To)));
    assert(result.second);
    (void) result;
  }

protected:

  SILBuilder Builder;
  SILBasicBlock *InsertBeforeBB;
  llvm::DenseMap<SILValue, SILValue> ValueMap;
  llvm::DenseMap<SILInstruction*, SILInstruction*> InstructionMap;

  // Use MapVector to ensure that the order of block predecessors is
  // deterministic.
  llvm::MapVector<SILBasicBlock*, SILBasicBlock*> BBMap;

  TypeSubstitutionMap OpenedExistentialSubs;
  SILOpenedArchetypesTracker OpenedArchetypesTracker;

  /// Set of basic blocks where unreachable was inserted.
  SmallPtrSet<SILBasicBlock *, 32> BlocksWithUnreachables;
};

/// \brief A SILBuilder that automatically invokes postprocess on each
/// inserted instruction.
template<class SomeSILCloner, unsigned N = 4>
class SILBuilderWithPostProcess : public SILBuilder {
  SomeSILCloner &SC;
  SILInstruction *Orig;
  SmallVector<SILInstruction*, N> InsertedInstrs;

public:
  SILBuilderWithPostProcess(SomeSILCloner *sc, SILInstruction *Orig)
    : SILBuilder(sc->getBuilder().getInsertionBB(), &InsertedInstrs),
      SC(*sc), Orig(Orig)
    {
      setInsertionPoint(SC.getBuilder().getInsertionBB(),
                        SC.getBuilder().getInsertionPoint());
      setOpenedArchetypesTracker(SC.getBuilder().getOpenedArchetypesTracker());
    }

  ~SILBuilderWithPostProcess() {
    for (auto *I : InsertedInstrs) {
      SC.doPostProcess(Orig, I);
    }
  }
};


/// SILClonerWithScopes - a SILCloner that automatically clones
/// SILDebugScopes. In contrast to inline scopes, this generates a
/// deep copy of the scope tree.
template<typename ImplClass>
class SILClonerWithScopes : public SILCloner<ImplClass> {
  friend class SILCloner<ImplClass>;
public:
  SILClonerWithScopes(SILFunction &To,
                      SILOpenedArchetypesTracker &OpenedArchetypesTracker,
                      bool Disable = false)
      : SILCloner<ImplClass>(To, OpenedArchetypesTracker) {

    // We only want to do this when we generate cloned functions, not
    // when we inline.

    // FIXME: This is due to having TypeSubstCloner inherit from
    //        SILClonerWithScopes, and having TypeSubstCloner be used
    //        both by passes that clone whole functions and ones that
    //        inline functions.
    if (Disable)
      return;

    scopeCloner.reset(new ScopeCloner(To));
  }

  SILClonerWithScopes(SILFunction &To,
                      bool Disable = false)
      : SILCloner<ImplClass>(To) {

    // We only want to do this when we generate cloned functions, not
    // when we inline.

    // FIXME: This is due to having TypeSubstCloner inherit from
    //        SILClonerWithScopes, and having TypeSubstCloner be used
    //        both by passes that clone whole functions and ones that
    //        inline functions.
    if (Disable)
      return;

    scopeCloner.reset(new ScopeCloner(To));
  }


private:
  std::unique_ptr<ScopeCloner> scopeCloner;
protected:
  /// Clone the SILDebugScope for the cloned function.
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    SILCloner<ImplClass>::postProcess(Orig, Cloned);
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    return scopeCloner ? scopeCloner->getOrCreateClonedScope(DS) : DS;
  }
};

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::remapValue(SILValue Value) {
  auto VI = ValueMap.find(Value);
  if (VI != ValueMap.end())
    return VI->second;

  if (SILInstruction* I = dyn_cast<SILInstruction>(Value)) {
    auto II = InstructionMap.find(I);
    if (II != InstructionMap.end())
      return SILValue(II->second);
    llvm_unreachable("Unmapped instruction while cloning?");
  }

  // If we have undef, just remap the type.
  if (SILUndef *U = dyn_cast<SILUndef>(Value)) {
    auto type = getOpType(U->getType());
    ValueBase *undef =
      (type == U->getType() ? U : SILUndef::get(type, Builder.getModule()));
    return SILValue(undef);
  }

  llvm_unreachable("Unmapped value while cloning?");
}

template<typename ImplClass>
SILBasicBlock*
SILCloner<ImplClass>::remapBasicBlock(SILBasicBlock *BB) {
  SILBasicBlock *MappedBB = BBMap[BB];
  assert(MappedBB && "Unmapped basic block while cloning?");
  return MappedBB;
}

template<typename ImplClass>
void
SILCloner<ImplClass>::postProcess(SILInstruction *Orig,
                                  SILInstruction *Cloned) {
  assert((Orig->getDebugScope() ? Cloned->getDebugScope()!=nullptr : true) &&
         "cloned function dropped debug scope");
  InstructionMap.insert(std::make_pair(Orig, Cloned));
}

/// \brief Recursively visit a callee's BBs in depth-first preorder (only
/// processing blocks on the first visit), mapping newly visited BBs to new BBs
/// in the caller and cloning all instructions into the caller other than
/// terminators which should be handled separately later by subclasses
template<typename ImplClass>
void
SILCloner<ImplClass>::visitSILBasicBlock(SILBasicBlock* BB) {
  SILFunction &F = getBuilder().getFunction();
  // Iterate over and visit all instructions other than the terminator to clone.
  for (auto I = BB->begin(), E = --BB->end(); I != E; ++I) {
    asImpl().visit(&*I);
  }
  // Iterate over successors to do the depth-first search.
  for (auto &Succ : BB->getSuccessors()) {
    auto BBI = BBMap.find(Succ);
    // Only visit a successor that has not already been visited.
    if (BBI == BBMap.end()) {
      // Map the successor to a new BB.
      auto *MappedBB = F.createBasicBlock();
      BBMap.insert(std::make_pair(Succ.getBB(), MappedBB));
      // Create new arguments for each of the original block's arguments.
      for (auto *Arg : Succ.getBB()->getPHIArguments()) {
        SILValue MappedArg = MappedBB->createPHIArgument(
            getOpType(Arg->getType()), Arg->getOwnershipKind());

        ValueMap.insert(std::make_pair(Arg, MappedArg));
      }
      // Also, move the new mapped BB to the right position in the caller
      if (InsertBeforeBB)
        F.getBlocks().splice(SILFunction::iterator(InsertBeforeBB),
                             F.getBlocks(), SILFunction::iterator(MappedBB));
      // Set the insertion point to the new mapped BB
      getBuilder().setInsertionPoint(MappedBB);
      // Recurse into the successor
      visitSILBasicBlock(Succ.getBB());
    }
  }
}

/// \brief Clean-up after cloning.
template<typename ImplClass>
void
SILCloner<ImplClass>::cleanUp(SILFunction *F) {

  // Remove any code after unreachable instructions.

  // NOTE: It is unfortunate that it essentially duplicates
  // the code from sil-combine, but doing so allows for
  // avoiding any cross-layer invocations between SIL and
  // SILOptimizer layers.

  for (auto *BB : BlocksWithUnreachables) {
    for (auto &I : *BB) {
      if (!isa<UnreachableInst>(&I))
        continue;

      // Collect together all the instructions after this point
      llvm::SmallVector<SILInstruction *, 32> ToRemove;
      for (auto Inst = BB->rbegin(); &*Inst != &I; ++Inst)
        ToRemove.push_back(&*Inst);

      for (auto *Inst : ToRemove) {
        // Replace any non-dead results with SILUndef values
        Inst->replaceAllUsesWithUndef();
        Inst->eraseFromParent();
      }
    }
  }

  BlocksWithUnreachables.clear();
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSILFunction(SILFunction *F) {
  for (auto &BB : *F)
    asImpl().visitSILBasicBlock(&BB);
  cleanUp(F);
}


template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocStackInst(AllocStackInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAllocStack(getOpLocation(Inst->getLoc()),
                                  getOpType(Inst->getElementType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocRefInst(AllocRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  auto CountArgs = getOpValueArray<8>(OperandValueArrayRef(Inst->
                                                    getTailAllocatedCounts()));
  SmallVector<SILType, 4> ElemTypes;
  for (SILType OrigElemType : Inst->getTailAllocatedTypes()) {
    ElemTypes.push_back(getOpType(OrigElemType));
  }
  auto *NewInst = getBuilder().createAllocRef(getOpLocation(Inst->getLoc()),
                                      getOpType(Inst->getType()),
                                      Inst->isObjC(), Inst->canAllocOnStack(),
                                      ElemTypes, CountArgs);
  doPostProcess(Inst, NewInst);
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocRefDynamicInst(AllocRefDynamicInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  auto CountArgs = getOpValueArray<8>(OperandValueArrayRef(Inst->
                                                  getTailAllocatedCounts()));
  SmallVector<SILType, 4> ElemTypes;
  for (SILType OrigElemType : Inst->getTailAllocatedTypes()) {
    ElemTypes.push_back(getOpType(OrigElemType));
  }
  auto *NewInst = getBuilder().createAllocRefDynamic(
                                      getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getMetatypeOperand()),
                                      getOpType(Inst->getType()),
                                      Inst->isObjC(),
                                      ElemTypes, CountArgs);
  doPostProcess(Inst, NewInst);
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocBoxInst(AllocBoxInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAllocBox(getOpLocation(Inst->getLoc()),
               this->getOpType(Inst->getType()).template castTo<SILBoxType>()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocExistentialBoxInst(
                                                AllocExistentialBoxInst *Inst) {
  auto origExistentialType = Inst->getExistentialType();
  auto origFormalType = Inst->getFormalConcreteType();

  auto conformances = getOpConformances(origFormalType,
                                        Inst->getConformances());

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAllocExistentialBox(getOpLocation(Inst->getLoc()),
                                getOpType(origExistentialType),
                                getOpASTType(origFormalType),
                                conformances));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocValueBufferInst(AllocValueBufferInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAllocValueBuffer(getOpLocation(Inst->getLoc()),
                                        getOpType(Inst->getValueType()),
                                        getOpValue(Inst->getOperand())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitBuiltinInst(BuiltinInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
        getBuilder().createBuiltin(getOpLocation(Inst->getLoc()),
                                   Inst->getName(),
                                   getOpType(Inst->getType()),
                                   getOpSubstitutions(Inst->getSubstitutions()),
                                   Args));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitApplyInst(ApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createApply(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getCallee()),
                             getOpType(Inst->getSubstCalleeSILType()),
                             getOpType(Inst->getType()),
                             getOpSubstitutions(Inst->getSubstitutions()),
                             Args,
                             Inst->isNonThrowing()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTryApplyInst(TryApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createTryApply(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getCallee()),
                                getOpType(Inst->getSubstCalleeSILType()),
                                getOpSubstitutions(Inst->getSubstitutions()),
                                Args,
                                getOpBasicBlock(Inst->getNormalBB()),
                                getOpBasicBlock(Inst->getErrorBB())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPartialApplyInst(PartialApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createPartialApply(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getCallee()),
                                    getOpType(Inst->getSubstCalleeSILType()),
                                    getOpSubstitutions(Inst->getSubstitutions()),
                                    Args,
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFunctionRefInst(FunctionRefInst *Inst) {
  SILFunction *OpFunction = getOpFunction(Inst->getReferencedFunction());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createFunctionRef(getOpLocation(Inst->getLoc()),
                                   OpFunction));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocGlobalInst(AllocGlobalInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAllocGlobal(getOpLocation(Inst->getLoc()),
                                   Inst->getReferencedGlobal()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitGlobalAddrInst(GlobalAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createGlobalAddr(getOpLocation(Inst->getLoc()),
                                  Inst->getReferencedGlobal()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIntegerLiteralInst(IntegerLiteralInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIntegerLiteral(getOpLocation(Inst->getLoc()),
                                      getOpType(Inst->getType()),
                                      Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFloatLiteralInst(FloatLiteralInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createFloatLiteral(getOpLocation(Inst->getLoc()),
                                    getOpType(Inst->getType()),
                                    Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStringLiteralInst(StringLiteralInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createStringLiteral(getOpLocation(Inst->getLoc()),
                                                 Inst->getValue(),
                                                 Inst->getEncoding()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadInst(LoadInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createLoad(getOpLocation(Inst->getLoc()),
                                              getOpValue(Inst->getOperand()),
                                              Inst->getOwnershipQualifier()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitLoadBorrowInst(LoadBorrowInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createLoadBorrow(getOpLocation(Inst->getLoc()),
                                              getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitBeginBorrowInst(BeginBorrowInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createBeginBorrow(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitStoreInst(StoreInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createStore(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getSrc()),
                                               getOpValue(Inst->getDest()),
                                               Inst->getOwnershipQualifier()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitStoreBorrowInst(StoreBorrowInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createStoreBorrow(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getSrc()),
                                               getOpValue(Inst->getDest())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitEndBorrowInst(EndBorrowInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createEndBorrow(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getBorrowedValue()),
                                         getOpValue(Inst->getOriginalValue())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitEndBorrowArgumentInst(
    EndBorrowArgumentInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createEndBorrowArgument(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitAssignInst(AssignInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAssign(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getSrc()),
                              getOpValue(Inst->getDest())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkUninitializedInst(MarkUninitializedInst *Inst) {
  SILValue OpValue = getOpValue(Inst->getOperand());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
             getBuilder().createMarkUninitialized(getOpLocation(Inst->getLoc()),
                                                  OpValue,
                                                  Inst->getKind()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkUninitializedBehaviorInst(
                                          MarkUninitializedBehaviorInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
     getBuilder().createMarkUninitializedBehavior(
                       getOpLocation(Inst->getLoc()),
                       getOpValue(Inst->getInitStorageFunc()),
                       getOpSubstitutions(Inst->getInitStorageSubstitutions()),
                       getOpValue(Inst->getStorage()),
                       getOpValue(Inst->getSetterFunc()),
                       getOpSubstitutions(Inst->getSetterSubstitutions()),
                       getOpValue(Inst->getSelf()),
                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *Inst){
  auto OpElements = getOpValueArray<8>(Inst->getElements());
  auto OpLoc = getOpLocation(Inst->getLoc());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createMarkFunctionEscape(OpLoc,
                                                      OpElements));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueInst(DebugValueInst *Inst) {
  // We cannot inline/clone debug intrinsics without a scope. If they
  // describe function arguments there is no way to determine which
  // function they belong to.
  if (!Inst->getDebugScope())
    return;

  // Since we want the debug info to survive, we do not remap the location here.
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createDebugValue(
                          Inst->getLoc(), getOpValue(Inst->getOperand()),
                          Inst->getVarInfo()));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  // We cannot inline/clone debug intrinsics without a scope. If they
  // describe function arguments there is no way to determine which
  // function they belong to.
  if (!Inst->getDebugScope())
    return;

  // Do not remap the location for a debug Instruction.
  SILValue OpValue = getOpValue(Inst->getOperand());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createDebugValueAddr(
                          Inst->getLoc(), OpValue, Inst->getVarInfo()));
}


template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadUnownedInst(LoadUnownedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createLoadUnowned(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->isTake()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreUnownedInst(StoreUnownedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStoreUnowned(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getSrc()),
                                    getOpValue(Inst->getDest()),
                                    Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadWeakInst(LoadWeakInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createLoadWeak(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->isTake()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreWeakInst(StoreWeakInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStoreWeak(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getSrc()),
                                 getOpValue(Inst->getDest()),
                                 Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyAddrInst(CopyAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createCopyAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getSrc()),
                                getOpValue(Inst->getDest()),
                                Inst->isTakeOfSrc(),
                                Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBindMemoryInst(BindMemoryInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createBindMemory(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getBase()),
                                  getOpValue(Inst->getIndex()),
                                  getOpType(Inst->getBoundType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitConvertFunctionInst(ConvertFunctionInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createConvertFunction(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitThinFunctionToPointerInst(
                                           ThinFunctionToPointerInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createThinFunctionToPointer(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             getOpType(Inst->getType())));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitPointerToThinFunctionInst(
                                           PointerToThinFunctionInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createPointerToThinFunction(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastInst(UpcastInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUpcast(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAddressToPointerInst(AddressToPointerInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAddressToPointer(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPointerToAddressInst(PointerToAddressInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createPointerToAddress(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType()),
                                        Inst->isStrict(),
                                        Inst->isInvariant()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedRefCastInst(UncheckedRefCastInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedRefCast(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *Inst) {
  SILLocation OpLoc = getOpLocation(Inst->getLoc());
  SILValue SrcValue = getOpValue(Inst->getSrc());
  SILValue DestValue = getOpValue(Inst->getDest());
  CanType SrcType = getOpASTType(Inst->getSourceType());
  CanType TargetType = getOpASTType(Inst->getTargetType());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().
                createUncheckedRefCastAddr(OpLoc, SrcValue, SrcType,
                                           DestValue, TargetType));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedAddrCastInst(UncheckedAddrCastInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedAddrCast(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedTrivialBitCast(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getOperand()),
                                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedBitwiseCast(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitRefToBridgeObjectInst(RefToBridgeObjectInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefToBridgeObject(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getConverted()),
                                         getOpValue(Inst->getBitsOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitBridgeObjectToRefInst(BridgeObjectToRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createBridgeObjectToRef(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getConverted()),
                                         getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitBridgeObjectToWordInst(BridgeObjectToWordInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createBridgeObjectToWord(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getConverted()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToRawPointerInst(RefToRawPointerInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefToRawPointer(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRawPointerToRefInst(RawPointerToRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRawPointerToRef(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToUnownedInst(RefToUnownedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefToUnowned(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedToRefInst(UnownedToRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUnownedToRef(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToUnmanagedInst(RefToUnmanagedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefToUnmanaged(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnmanagedToRefInst(UnmanagedToRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUnmanagedToRef(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitThinToThickFunctionInst(ThinToThickFunctionInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createThinToThickFunction(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createThickToObjCMetatype(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createObjCToThickMetatype(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIsNonnullInst(IsNonnullInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIsNonnull(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnconditionalCheckedCastInst(
                                          UnconditionalCheckedCastInst *Inst) {
  SILLocation OpLoc = getOpLocation(Inst->getLoc());
  SILValue OpValue = getOpValue(Inst->getOperand());
  SILType OpType = getOpType(Inst->getType());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
         getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                     OpValue,
                                                     OpType));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnconditionalCheckedCastAddrInst(
                                      UnconditionalCheckedCastAddrInst *Inst) {
  SILLocation OpLoc = getOpLocation(Inst->getLoc());
  SILValue SrcValue = getOpValue(Inst->getSrc());
  SILValue DestValue = getOpValue(Inst->getDest());
  CanType SrcType = getOpASTType(Inst->getSourceType());
  CanType TargetType = getOpASTType(Inst->getTargetType());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
         getBuilder().createUnconditionalCheckedCastAddr(OpLoc,
                                                    Inst->getConsumptionKind(),
                                                         SrcValue, SrcType,
                                                         DestValue, TargetType));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitUnconditionalCheckedCastValueInst(
    UnconditionalCheckedCastValueInst *Inst) {
  SILLocation OpLoc = getOpLocation(Inst->getLoc());
  SILValue OpValue = getOpValue(Inst->getOperand());
  SILType OpType = getOpType(Inst->getType());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createUnconditionalCheckedCastValue(
                          OpLoc, Inst->getConsumptionKind(), OpValue, OpType));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitRetainValueInst(RetainValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRetainValue(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getAtomicity()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitUnmanagedRetainValueInst(
    UnmanagedRetainValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createUnmanagedRetainValue(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand()),
                  Inst->getAtomicity()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitCopyValueInst(CopyValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createCopyValue(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitCopyUnownedValueInst(
    CopyUnownedValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createCopyUnownedValue(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitReleaseValueInst(ReleaseValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createReleaseValue(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getAtomicity()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitUnmanagedReleaseValueInst(
    UnmanagedReleaseValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createUnmanagedReleaseValue(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand()),
                  Inst->getAtomicity()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitDestroyValueInst(DestroyValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createDestroyValue(getOpLocation(Inst->getLoc()),
                                            getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitAutoreleaseValueInst(
    AutoreleaseValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createAutoreleaseValue(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getAtomicity()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitUnmanagedAutoreleaseValueInst(
    UnmanagedAutoreleaseValueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createUnmanagedAutoreleaseValue(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand()),
                  Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSetDeallocatingInst(SetDeallocatingInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSetDeallocating(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructInst(StructInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStruct(getOpLocation(Inst->getLoc()),
                              getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleInst(TupleInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createTuple(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitEnumInst(EnumInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createEnum(getOpLocation(Inst->getLoc()),
                            Inst->hasOperand() ? getOpValue(Inst->getOperand())
                                               : SILValue(),
                            Inst->getElement(),
                            getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitEnumDataAddrInst(InitEnumDataAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createInitEnumDataAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getElement(),
                                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitUncheckedEnumDataInst(UncheckedEnumDataInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedEnumData(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         Inst->getElement(),
                                         getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUncheckedTakeEnumDataAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getElement(),
                                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitInjectEnumAddrInst(InjectEnumAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createInjectEnumAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getElement()));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitMetatypeInst(MetatypeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createMetatype(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitValueMetatypeInst(ValueMetatypeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createValueMetatype(getOpLocation(Inst->getLoc()),
                                     getOpType(Inst->getType()),
                                     getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitExistentialMetatypeInst(ExistentialMetatypeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createExistentialMetatype(getOpLocation(Inst->getLoc()),
                                           getOpType(Inst->getType()),
                                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleExtractInst(TupleExtractInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createTupleExtract(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getFieldNo(),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleElementAddrInst(TupleElementAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createTupleElementAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getFieldNo(),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructExtractInst(StructExtractInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStructExtract(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getField(),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructElementAddrInst(StructElementAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStructElementAddr(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         Inst->getField(),
                                         getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefElementAddrInst(RefElementAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefElementAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getField(),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefTailAddrInst(RefTailAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createRefTailAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitClassMethodInst(ClassMethodInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createClassMethod(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getMember(),
                                   Inst->getType(),
                                   Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSuperMethodInst(SuperMethodInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSuperMethod(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getMember(),
                                   Inst->getType(),
                                   Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitWitnessMethodInst(WitnessMethodInst *Inst) {
  auto lookupType = Inst->getLookupType();
  auto conformance = getOpConformance(lookupType, Inst->getConformance());
  auto newLookupType = getOpASTType(lookupType);

  if (conformance.isConcrete()) {
    CanType Ty = conformance.getConcrete()->getType()->getCanonicalType();

    if (Ty != newLookupType) {
      assert(Ty->isExactSuperclassOf(newLookupType, nullptr) &&
             "Should only create upcasts for sub class.");

      // We use the super class as the new look up type.
      newLookupType = Ty;
    }
  }

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst,
      getBuilder()
          .createWitnessMethod(
              getOpLocation(Inst->getLoc()),
              newLookupType, conformance,
              Inst->getMember(), Inst->getType(),
              Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDynamicMethodInst(DynamicMethodInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDynamicMethod(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getMember(),
                                     getOpType(Inst->getType()),
                                     Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitOpenExistentialAddrInst(OpenExistentialAddrInst *Inst) {
  // Create a new archetype for this opened existential type.
  auto archetypeTy
    = Inst->getType().getSwiftRValueType()->castTo<ArchetypeType>();
  registerOpenedExistentialRemapping(
      archetypeTy,
      ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType()));

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createOpenExistentialAddr(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType()),
                                       Inst->getAccessKind()));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitOpenExistentialOpaqueInst(
    OpenExistentialOpaqueInst *Inst) {
  // Create a new archetype for this opened existential type.
  auto archetypeTy =
      Inst->getType().getSwiftRValueType()->castTo<ArchetypeType>();
  registerOpenedExistentialRemapping(
      archetypeTy,
      ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType()));

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createOpenExistentialOpaque(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()),
                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitOpenExistentialMetatypeInst(OpenExistentialMetatypeInst *Inst) {
  // Create a new archetype for this opened existential type.
  CanType openedType = Inst->getType().getSwiftRValueType();
  CanType exType = Inst->getOperand()->getType().getSwiftRValueType();
  while (auto exMetatype = dyn_cast<ExistentialMetatypeType>(exType)) {
    exType = exMetatype.getInstanceType();
    openedType = cast<MetatypeType>(openedType).getInstanceType();
  }
  auto archetypeTy = cast<ArchetypeType>(openedType);
  registerOpenedExistentialRemapping(
      archetypeTy,
      ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType()));

  if (!Inst->getOperand()->getType().canUseExistentialRepresentation(
          Inst->getModule(), ExistentialRepresentation::Class)) {
    getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createOpenExistentialMetatype(
                            getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            getOpType(Inst->getType())));
    return;
  }

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createOpenExistentialMetatype(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getOperand()),
                                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitOpenExistentialRefInst(OpenExistentialRefInst *Inst) {
  // Create a new archetype for this opened existential type.
  auto archetypeTy
    = Inst->getType().getSwiftRValueType()->castTo<ArchetypeType>();
  registerOpenedExistentialRemapping(
      archetypeTy,
      ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType()));

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createOpenExistentialRef(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitOpenExistentialBoxInst(OpenExistentialBoxInst *Inst) {
  // Create a new archetype for this opened existential type.
  auto archetypeTy
    = Inst->getType().getSwiftRValueType()->castTo<ArchetypeType>();
  registerOpenedExistentialRemapping(
      archetypeTy,
      ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType()));

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createOpenExistentialBox(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitExistentialAddrInst(InitExistentialAddrInst *Inst) {
  CanType origFormalType = Inst->getFormalConcreteType();

  auto conformances = getOpConformances(origFormalType,
                                        Inst->getConformances());

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createInitExistentialAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpASTType(origFormalType),
                                   getOpType(Inst->getLoweredConcreteType()),
                                       conformances));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitInitExistentialOpaqueInst(
    InitExistentialOpaqueInst *Inst) {
  CanType origFormalType = Inst->getFormalConcreteType();

  auto conformances = getOpConformances(origFormalType,
                                        Inst->getConformances());

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createInitExistentialOpaque(
                    getOpLocation(Inst->getLoc()), getOpType(Inst->getType()),
                    getOpASTType(origFormalType),
                    getOpValue(Inst->getOperand()), conformances));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *Inst) {
  auto origFormalType = Inst->getFormalErasedObjectType();
  auto conformances = getOpConformances(origFormalType,
                                        Inst->getConformances());

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createInitExistentialMetatype(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getOperand()),
                                               getOpType(Inst->getType()),
                                               conformances));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitInitExistentialRefInst(InitExistentialRefInst *Inst) {
  CanType origFormalType = Inst->getFormalConcreteType();
  auto conformances = getOpConformances(origFormalType,
                                        Inst->getConformances());

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createInitExistentialRef(getOpLocation(Inst->getLoc()),
                                    getOpType(Inst->getType()),
                                    getOpASTType(origFormalType),
                                    getOpValue(Inst->getOperand()),
                                    conformances));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeinitExistentialAddrInst(DeinitExistentialAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeinitExistentialAddr(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitDeinitExistentialOpaqueInst(
    DeinitExistentialOpaqueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(
      Inst, getBuilder().createDeinitExistentialOpaque(
                getOpLocation(Inst->getLoc()), getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyBlockInst(CopyBlockInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    Builder.createCopyBlock(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongRetainInst(StrongRetainInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStrongRetain(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFixLifetimeInst(FixLifetimeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createFixLifetime(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitEndLifetimeInst(EndLifetimeInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
                getBuilder().createEndLifetime(getOpLocation(Inst->getLoc()),
                                               getOpValue(Inst->getOperand())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitUncheckedOwnershipConversionInst(
    UncheckedOwnershipConversionInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  ValueOwnershipKind Kind = SILValue(Inst).getOwnershipKind();
  if (getOpValue(Inst->getOperand()).getOwnershipKind() ==
      ValueOwnershipKind::Trivial) {
    Kind = ValueOwnershipKind::Trivial;
  }
  doPostProcess(Inst, getBuilder().createUncheckedOwnershipConversion(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()), Kind));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitMarkDependenceInst(MarkDependenceInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createMarkDependence(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getValue()),
                                      getOpValue(Inst->getBase())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongPinInst(StrongPinInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStrongPin(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand()),
                                 Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongUnpinInst(StrongUnpinInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStrongUnpin(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStrongRelease(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitStrongRetainUnownedInst(StrongRetainUnownedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createStrongRetainUnowned(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedRetainInst(UnownedRetainInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUnownedRetain(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getAtomicity()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedReleaseInst(UnownedReleaseInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUnownedRelease(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getAtomicity()));
}
template<typename ImplClass>
void SILCloner<ImplClass>::visitIsUniqueInst(IsUniqueInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIsUnique(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::
visitIsUniqueOrPinnedInst(IsUniqueOrPinnedInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIsUniqueOrPinned(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocStackInst(DeallocStackInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocStack(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocRefInst(DeallocRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocRef(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  Inst->canAllocOnStack()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocPartialRefInst(DeallocPartialRefInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocPartialRef(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getInstance()),
                                         getOpValue(Inst->getMetatype())));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitDeallocValueBufferInst(
                                              DeallocValueBufferInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocValueBuffer(getOpLocation(Inst->getLoc()),
                                          getOpType(Inst->getValueType()),
                                          getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocBoxInst(DeallocBoxInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocBox(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocExistentialBoxInst(
                                              DeallocExistentialBoxInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDeallocExistentialBox(getOpLocation(Inst->getLoc()),
                                         getOpASTType(Inst->getConcreteType()),
                                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDestroyAddrInst(DestroyAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createDestroyAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitProjectValueBufferInst(
                                              ProjectValueBufferInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createProjectValueBuffer(getOpLocation(Inst->getLoc()),
                                          getOpType(Inst->getValueType()),
                                          getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitProjectBoxInst(ProjectBoxInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createProjectBox(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  Inst->getFieldIndex()));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitProjectExistentialBoxInst(
                                            ProjectExistentialBoxInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
      getBuilder().createProjectExistentialBox(getOpLocation(Inst->getLoc()),
                                               getOpType(Inst->getType()),
                                               getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondFailInst(CondFailInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createCondFail(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexAddrInst(IndexAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIndexAddr(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getBase()),
                                 getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTailAddrInst(TailAddrInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createTailAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getBase()),
                                getOpValue(Inst->getIndex()),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexRawPointerInst(IndexRawPointerInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createIndexRawPointer(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getBase()),
                                       getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnreachableInst(UnreachableInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createUnreachable(getOpLocation(Inst->getLoc())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitReturnInst(ReturnInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createReturn(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitThrowInst(ThrowInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createThrow(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBranchInst(BranchInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArgs());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createBranch(getOpLocation(Inst->getLoc()),
                              getOpBasicBlock(Inst->getDestBB()), Args));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondBranchInst(CondBranchInst *Inst) {
  auto TrueArgs = getOpValueArray<8>(Inst->getTrueArgs());
  auto FalseArgs = getOpValueArray<8>(Inst->getFalseArgs());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createCondBranch(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getCondition()),
                                  getOpBasicBlock(Inst->getTrueBB()), TrueArgs,
                                  getOpBasicBlock(Inst->getFalseBB()),
                                  FalseArgs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
  SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
       getBuilder().createCheckedCastBranch(getOpLocation(Inst->getLoc()),
                                            Inst->isExact(),
                                            getOpValue(Inst->getOperand()),
                                            getOpType(Inst->getCastType()),
                                            OpSuccBB, OpFailBB));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitCheckedCastValueBranchInst(
    CheckedCastValueBranchInst *Inst) {
  SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
  SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createCheckedCastValueBranch(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()),
                          getOpType(Inst->getCastType()), OpSuccBB, OpFailBB));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitCheckedCastAddrBranchInst(
                                             CheckedCastAddrBranchInst *Inst) {
  SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
  SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
  SILValue SrcValue = getOpValue(Inst->getSrc());
  SILValue DestValue = getOpValue(Inst->getDest());
  CanType SrcType = getOpASTType(Inst->getSourceType());
  CanType TargetType = getOpASTType(Inst->getTargetType());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
       getBuilder().createCheckedCastAddrBranch(getOpLocation(Inst->getLoc()),
                                                Inst->getConsumptionKind(),
                                                SrcValue, SrcType,
                                                DestValue, TargetType,
                                                OpSuccBB, OpFailBB));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitSwitchValueInst(SwitchValueInst *Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<SILValue, SILBasicBlock*>, 8> CaseBBs;
  for (int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(getOpValue(Inst->getCase(i).first),
                                     getOpBasicBlock(Inst->getCase(i).second)));
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSwitchValue(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   DefaultBB, CaseBBs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSwitchEnumInst(SwitchEnumInst *Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> CaseBBs;
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSwitchEnum(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  DefaultBB, CaseBBs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitSwitchEnumAddrInst(SwitchEnumAddrInst *Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> CaseBBs;
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSwitchEnumAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      DefaultBB, CaseBBs));
}
  

  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitSelectEnumInst(SelectEnumInst *Inst) {
  SILValue DefaultResult;
  if (Inst->hasDefault())
    DefaultResult = getOpValue(Inst->getDefaultResult());
  SmallVector<std::pair<EnumElementDecl*, SILValue>, 8> CaseResults;
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseResults.push_back(std::make_pair(Inst->getCase(i).first,
                                         getOpValue(Inst->getCase(i).second)));
  
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSelectEnum(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getEnumOperand()),
                                  getOpType(Inst->getType()),
                                  DefaultResult, CaseResults));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSelectEnumAddrInst(SelectEnumAddrInst *Inst) {
  SILValue DefaultResult;
  if (Inst->hasDefault())
    DefaultResult = getOpValue(Inst->getDefaultResult());
  SmallVector<std::pair<EnumElementDecl*, SILValue>, 8> CaseResults;
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseResults.push_back(std::make_pair(Inst->getCase(i).first,
                                         getOpValue(Inst->getCase(i).second)));
  
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSelectEnumAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getEnumOperand()),
                                      getOpType(Inst->getType()),
                                      DefaultResult, CaseResults));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSelectValueInst(SelectValueInst *Inst) {
  SILValue DefaultResult;
  if (Inst->hasDefault())
    DefaultResult = getOpValue(Inst->getDefaultResult());
  SmallVector<std::pair<SILValue, SILValue>, 8> CaseResults;
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseResults.push_back(std::make_pair(getOpValue(Inst->getCase(i).first),
                                         getOpValue(Inst->getCase(i).second)));

  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst,
    getBuilder().createSelectValue(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType()),
                                   DefaultResult, CaseResults));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitDynamicMethodBranchInst(
    DynamicMethodBranchInst *Inst) {
  SILBasicBlock *OpHasMethodBB = getOpBasicBlock(Inst->getHasMethodBB());
  SILBasicBlock *OpHasNoMethodBB = getOpBasicBlock(Inst->getNoMethodBB());
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createDynamicMethodBranch(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()), Inst->getMember(),
                          OpHasMethodBB, OpHasNoMethodBB));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitProjectBlockStorageInst(
    ProjectBlockStorageInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createProjectBlockStorage(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()),
                          getOpType(Inst->getType())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitInitBlockStorageHeaderInst(
    InitBlockStorageHeaderInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createInitBlockStorageHeader(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getBlockStorage()),
                          getOpValue(Inst->getInvokeFunction()),
                          getOpType(Inst->getType()),
                          getOpSubstitutions(Inst->getSubstitutions())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitObjCMetatypeToObjectInst(
    ObjCMetatypeToObjectInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createObjCMetatypeToObject(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()),
                          getOpType(Inst->getType())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitObjCExistentialMetatypeToObjectInst(
    ObjCExistentialMetatypeToObjectInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createObjCExistentialMetatypeToObject(
                          getOpLocation(Inst->getLoc()),
                          getOpValue(Inst->getOperand()),
                          getOpType(Inst->getType())));
}

template <typename ImplClass>
void SILCloner<ImplClass>::visitObjCProtocolInst(ObjCProtocolInst *Inst) {
  getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
  doPostProcess(Inst, getBuilder().createObjCProtocol(
                          getOpLocation(Inst->getLoc()), Inst->getProtocol(),
                          getOpType(Inst->getType())));
}

} // end namespace swift

#endif
