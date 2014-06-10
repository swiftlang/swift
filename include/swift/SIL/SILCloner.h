//===--- SILCloner.h - Defines the SILCloner class ---------------*- C++ -*-==//
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
// This file defines the SILCloner class, used for cloning SIL instructions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILCLONER_H
#define SWIFT_SIL_SILCLONER_H

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"

namespace swift {

/// SILCloner - Abstract SIL visitor which knows how to clone instructions and
/// whose behavior can be customized by subclasses via the CRTP. This is meant
/// to be subclassed to implement inlining, function specialization, and other
/// operations requiring cloning (while possibly modifying, at the same time)
/// instruction sequences.
///
/// By default, this visitor will not do anything useful when when called on a
/// basic block, or function; subclasses that want to handle those should
/// implement the appropriate visit functions and/or provide other entry points.
template<typename ImplClass>
class SILCloner : protected SILVisitor<ImplClass> {
  friend class SILVisitor<ImplClass, SILValue>;
  using SILVisitor<ImplClass>::asImpl;

public:
  explicit SILCloner(SILFunction &F)
    : Builder(F), InsertBeforeBB(nullptr) { }
  
  /// Clients of SILCloner who want to know about any newly created
  /// instructions can install a SmallVector into the builder to collect them.
  void setTrackingList(SmallVectorImpl<SILInstruction*> *II) {
    getBuilder().setTrackingList(II);
  }
  
  SmallVectorImpl<SILInstruction*> *getTrackingList() {
    return getBuilder().getTrackingList();
  }


protected:
#define VALUE(CLASS, PARENT) \
  void visit##CLASS(CLASS *I) {                                       \
    llvm_unreachable("SILCloner visiting non-instruction?");          \
  }
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
  void visit##CLASS(CLASS *I);
#include "swift/SIL/SILNodes.def"

  void visitSILBasicBlock(SILBasicBlock* BB);

  SILBuilder &getBuilder() { return Builder; }

  // Derived classes of SILCloner using the CRTP can implement the following
  // functions to customize behavior; the remap functions are called before
  // cloning to modify constructor arguments and the post process function is
  // called afterwards on the result.
  SILLocation remapLocation(SILLocation Loc) { return Loc; }
  SILType remapType(SILType Ty) { return Ty; }
  ProtocolConformance *remapConformance(SILType Ty, ProtocolConformance *C) {
    return C;
  }
  SILValue remapValue(SILValue Value);
  SILFunction *remapFunction(SILFunction *Func) { return Func; }
  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB);
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned);

  SILLocation getOpLocation(SILLocation Loc) {
    return asImpl().remapLocation(Loc);
  }
  SILType getOpType(SILType Ty) {
    // Substitute opened existential types, if we have any.
    if (!OpenedExistentialSubs.empty()) {
      auto &F = getBuilder().getFunction();
      Ty = SILType::substType(F.getModule(), 
                              F.getModule().getSwiftModule(),
                              OpenedExistentialSubs,
                              Ty);
    }

    return asImpl().remapType(Ty);
  }
  ProtocolConformance *getOpConformance(SILType Ty,
                                        ProtocolConformance *Conformance) {
    return asImpl().remapConformance(Ty, Conformance);
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
  void doPostProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    asImpl().postProcess(Orig, Cloned);
  }

  SILBuilder Builder;
  SILBasicBlock *InsertBeforeBB;
  llvm::DenseMap<SILValue, SILValue> ValueMap;
  llvm::DenseMap<SILInstruction*, SILInstruction*> InstructionMap;
  llvm::DenseMap<SILBasicBlock*, SILBasicBlock*> BBMap;
  TypeSubstitutionMap OpenedExistentialSubs;
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
      return SILValue(II->second, Value.getResultNumber());
    llvm_unreachable("Unmapped instruction while cloning?");
  }

  // If we have undef, just copy it.
  if (SILUndef *U = dyn_cast<SILUndef>(Value))
    return SILValue(U, Value.getResultNumber());

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
  SILDebugScope *DebugScope = Orig->getDebugScope();
  if (DebugScope && !Cloned->getDebugScope())
    Cloned->setDebugScope(DebugScope);
  InstructionMap.insert(std::make_pair(Orig, Cloned));
}

// \brief Recursively visit a callee's BBs in depth-first preorder (only
/// processing blocks on the first visit), mapping newly visited BBs to new BBs
/// in the caller and cloning all instructions into the caller other than
/// terminators which should be handled separately later by subclasses
template<typename ImplClass>
void
SILCloner<ImplClass>::visitSILBasicBlock(SILBasicBlock* BB) {
  SILFunction &F = getBuilder().getFunction();
  // Iterate over and visit all instructions other than the terminator to clone.
  for (auto I = BB->begin(), E = --BB->end(); I != E; ++I)
    asImpl().visit(I);
  // Iterate over successors to do the depth-first search.
  for (auto &Succ : BB->getSuccs()) {
    auto BBI = BBMap.find(Succ);
    // Only visit a successor that has not already been visisted.
    if (BBI == BBMap.end()) {
      // Map the successor to a new BB.
      auto MappedBB = new (F.getModule()) SILBasicBlock(&F);
      BBMap.insert(std::make_pair(Succ.getBB(), MappedBB));
      // Create new arguments for each of the original block's arguments.
      for (auto &Arg : Succ.getBB()->getBBArgs()) {
        SILValue MappedArg =
          new (F.getModule()) SILArgument(getOpType(Arg->getType()), MappedBB);

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

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocStackInst(AllocStackInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAllocStack(getOpLocation(Inst->getLoc()),
                                  getOpType(Inst->getElementType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocRefInst(AllocRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAllocRef(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType()),
                                Inst->isObjC()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocRefDynamicInst(AllocRefDynamicInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAllocRefDynamic(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType()),
                                       Inst->isObjC()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocBoxInst(AllocBoxInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAllocBox(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getElementType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocArrayInst(AllocArrayInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAllocArray(getOpLocation(Inst->getLoc()),
                                  getOpType(Inst->getElementType()),
                                  getOpValue(Inst->getNumElements())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitApplyInst(ApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  doPostProcess(Inst,
    getBuilder().createApply(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getCallee()),
                             getOpType(Inst->getSubstCalleeSILType()),
                             getOpType(Inst->getType()),
                             Inst->getSubstitutions(), Args,
                             Inst->isTransparent()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPartialApplyInst(PartialApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  doPostProcess(Inst,
    getBuilder().createPartialApply(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getCallee()),
                                    getOpType(Inst->getSubstCalleeSILType()),
                                    Inst->getSubstitutions(), Args,
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *Inst){
  doPostProcess(Inst,
    getBuilder().createBuiltinFunctionRef(getOpLocation(Inst->getLoc()),
                                          Inst->getName(),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFunctionRefInst(FunctionRefInst *Inst) {
  SILFunction *OpFunction = getOpFunction(Inst->getReferencedFunction());
  doPostProcess(Inst,
    getBuilder().createFunctionRef(getOpLocation(Inst->getLoc()),
                                   OpFunction));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitGlobalAddrInst(GlobalAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createGlobalAddr(getOpLocation(Inst->getLoc()),
                                  Inst->getGlobal(),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSILGlobalAddrInst(SILGlobalAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createSILGlobalAddr(getOpLocation(Inst->getLoc()),
                                     Inst->getReferencedGlobal()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIntegerLiteralInst(IntegerLiteralInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createIntegerLiteral(getOpLocation(Inst->getLoc()),
                                      getOpType(Inst->getType()),
                                      Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFloatLiteralInst(FloatLiteralInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createFloatLiteral(getOpLocation(Inst->getLoc()),
                                    getOpType(Inst->getType()),
                                    Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStringLiteralInst(StringLiteralInst *Inst) {
  doPostProcess(Inst,
                getBuilder().createStringLiteral(getOpLocation(Inst->getLoc()),
                                                 Inst->getValue(),
                                                 Inst->getEncoding()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadInst(LoadInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createLoad(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreInst(StoreInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStore(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getSrc()),
                             getOpValue(Inst->getDest())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAssignInst(AssignInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAssign(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getSrc()),
                              getOpValue(Inst->getDest())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkUninitializedInst(MarkUninitializedInst *Inst) {
  SILValue OpValue = getOpValue(Inst->getOperand());
  doPostProcess(Inst,
             getBuilder().createMarkUninitialized(getOpLocation(Inst->getLoc()),
                                                  OpValue,
                                                  Inst->getKind()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *Inst){
  auto OpElements = getOpValueArray<8>(Inst->getElements());
  auto OpLoc = getOpLocation(Inst->getLoc());
  doPostProcess(Inst,
                getBuilder().createMarkFunctionEscape(OpLoc,
                                                      OpElements));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueInst(DebugValueInst *Inst) {
  // Since we want the debug info to survive, we do not remap the location here.
  doPostProcess(Inst,
                getBuilder().createDebugValue(Inst->getLoc(),
                                              getOpValue(Inst->getOperand())));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  // Do not remap the location for a debug instruction.
  SILValue OpValue = getOpValue(Inst->getOperand());
  doPostProcess(Inst,
                getBuilder().createDebugValueAddr(Inst->getLoc(),
                                                  OpValue));
}


template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadWeakInst(LoadWeakInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createLoadWeak(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->isTake()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreWeakInst(StoreWeakInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStoreWeak(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getSrc()),
                                 getOpValue(Inst->getDest()),
                                 Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyAddrInst(CopyAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createCopyAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getSrc()),
                                getOpValue(Inst->getDest()),
                                Inst->isTakeOfSrc(),
                                Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitConvertFunctionInst(ConvertFunctionInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createConvertFunction(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastInst(UpcastInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUpcast(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAddressToPointerInst(AddressToPointerInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAddressToPointer(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPointerToAddressInst(PointerToAddressInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createPointerToAddress(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedRefCastInst(UncheckedRefCastInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUncheckedRefCast(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUncheckedAddrCastInst(UncheckedAddrCastInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUncheckedAddrCast(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToRawPointerInst(RefToRawPointerInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRefToRawPointer(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRawPointerToRefInst(RawPointerToRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRawPointerToRef(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToUnownedInst(RefToUnownedInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRefToUnowned(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedToRefInst(UnownedToRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUnownedToRef(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToUnmanagedInst(RefToUnmanagedInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRefToUnmanaged(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnmanagedToRefInst(UnmanagedToRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUnmanagedToRef(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitThinToThickFunctionInst(ThinToThickFunctionInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createThinToThickFunction(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createThickToObjCMetatype(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createObjCToThickMetatype(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIsNonnullInst(IsNonnullInst *Inst) {
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
  doPostProcess(Inst,
         getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                     Inst->getCastKind(),
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
  doPostProcess(Inst,
         getBuilder().createUnconditionalCheckedCastAddr(OpLoc,
                                                         Inst->getCastKind(),
                                                         SrcValue,
                                                         DestValue));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitRetainValueInst(RetainValueInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRetainValue(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitReleaseValueInst(ReleaseValueInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createReleaseValue(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAutoreleaseValueInst(AutoreleaseValueInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAutoreleaseValue(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructInst(StructInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  doPostProcess(Inst,
    getBuilder().createStruct(getOpLocation(Inst->getLoc()),
                              getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleInst(TupleInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  doPostProcess(Inst,
    getBuilder().createTuple(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitEnumInst(EnumInst *Inst) {
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
  doPostProcess(Inst,
    getBuilder().createInitEnumDataAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getElement(),
                                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitUncheckedEnumDataInst(UncheckedEnumDataInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUncheckedEnumData(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         Inst->getElement(),
                                         getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUncheckedTakeEnumDataAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getElement(),
                                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitInjectEnumAddrInst(InjectEnumAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createInjectEnumAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getElement()));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitMetatypeInst(MetatypeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createMetatype(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitValueMetatypeInst(ValueMetatypeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createValueMetatype(getOpLocation(Inst->getLoc()),
                                     getOpType(Inst->getType()),
                                     getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitExistentialMetatypeInst(ExistentialMetatypeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createExistentialMetatype(getOpLocation(Inst->getLoc()),
                                           getOpType(Inst->getType()),
                                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleExtractInst(TupleExtractInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createTupleExtract(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getFieldNo(),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleElementAddrInst(TupleElementAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createTupleElementAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        Inst->getFieldNo(),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructExtractInst(StructExtractInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStructExtract(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getField(),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructElementAddrInst(StructElementAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStructElementAddr(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         Inst->getField(),
                                         getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefElementAddrInst(RefElementAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createRefElementAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getField(),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitClassMethodInst(ClassMethodInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createClassMethod(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getMember(),
                                   getOpType(Inst->getType()),
                                   Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSuperMethodInst(SuperMethodInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createSuperMethod(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getMember(),
                                   getOpType(Inst->getType()),
                                   Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitWitnessMethodInst(WitnessMethodInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createWitnessMethod(getOpLocation(Inst->getLoc()),
                                     getOpType(Inst->getLookupType()),
                                     getOpConformance(Inst->getLookupType(),
                                                      Inst->getConformance()),
                                     Inst->getMember(),
                                     getOpType(Inst->getType()),
                                     Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitProtocolMethodInst(ProtocolMethodInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createProtocolMethod(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getMember(),
                                      getOpType(Inst->getType()),
                                      Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDynamicMethodInst(DynamicMethodInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDynamicMethod(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getMember(),
                                     getOpType(Inst->getType()),
                                     Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitProjectExistentialInst(ProjectExistentialInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createProjectExistential(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitProjectExistentialRefInst(ProjectExistentialRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createProjectExistentialRef(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitOpenExistentialInst(OpenExistentialInst *Inst) {
  // Create a new archetype for this opened existential type.
  auto archetypeTy
    = Inst->getType().getSwiftRValueType()->castTo<ArchetypeType>();
  assert(OpenedExistentialSubs.count(archetypeTy) == 0 && 
         "Already substituted opened existential archetype?");
  OpenedExistentialSubs[archetypeTy] 
    = ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType());

  doPostProcess(Inst,
    getBuilder().createOpenExistential(getOpLocation(Inst->getLoc()),
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
  OpenedExistentialSubs[archetypeTy] 
    = ArchetypeType::getOpened(archetypeTy->getOpenedExistentialType());

  doPostProcess(Inst,
    getBuilder().createOpenExistentialRef(getOpLocation(Inst->getLoc()),
                                          getOpValue(Inst->getOperand()),
                                          getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitExistentialInst(InitExistentialInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createInitExistential(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getConcreteType()),
                                       Inst->getConformances()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitInitExistentialRefInst(InitExistentialRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createInitExistentialRef(getOpLocation(Inst->getLoc()),
                                          getOpType(Inst->getType()),
                                          getOpValue(Inst->getOperand()),
                                          Inst->getConformances()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeinitExistentialInst(DeinitExistentialInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDeinitExistential(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastExistentialInst(UpcastExistentialInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUpcastExistential(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getSrcExistential()),
                                         getOpValue(Inst->getDestExistential()),
                                         (IsTake_t)Inst->isTakeOfSrc()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitUpcastExistentialRefInst(UpcastExistentialRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUpcastExistentialRef(getOpLocation(Inst->getLoc()),
                                            getOpValue(Inst->getOperand()),
                                            getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyBlockInst(CopyBlockInst *Inst) {
  doPostProcess(Inst,
    Builder.createCopyBlock(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongRetainInst(StrongRetainInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStrongRetain(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *Inst) {
  SILValue OpValue = getOpValue(Inst->getOperand());
  doPostProcess(Inst,
    getBuilder().createStrongRetainAutoreleased(getOpLocation(Inst->getLoc()),
                                                OpValue));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFixLifetimeInst(FixLifetimeInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createFixLifetime(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStrongRelease(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitStrongRetainUnownedInst(StrongRetainUnownedInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createStrongRetainUnowned(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedRetainInst(UnownedRetainInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUnownedRetain(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedReleaseInst(UnownedReleaseInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUnownedRelease(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocStackInst(DeallocStackInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDeallocStack(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocRefInst(DeallocRefInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDeallocRef(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocBoxInst(DeallocBoxInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDeallocBox(getOpLocation(Inst->getLoc()),
                                  getOpType(Inst->getElementType()),
                                  getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDestroyAddrInst(DestroyAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createDestroyAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondFailInst(CondFailInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createCondFail(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexAddrInst(IndexAddrInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createIndexAddr(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getBase()),
                                 getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexRawPointerInst(IndexRawPointerInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createIndexRawPointer(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getBase()),
                                       getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnreachableInst(UnreachableInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createUnreachable(getOpLocation(Inst->getLoc())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitReturnInst(ReturnInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createReturn(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAutoreleaseReturnInst(AutoreleaseReturnInst *Inst) {
  doPostProcess(Inst,
    getBuilder().createAutoreleaseReturn(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBranchInst(BranchInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArgs());
  doPostProcess(Inst,
    getBuilder().createBranch(getOpLocation(Inst->getLoc()),
                              getOpBasicBlock(Inst->getDestBB()), Args));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondBranchInst(CondBranchInst *Inst) {
  auto TrueArgs = getOpValueArray<8>(Inst->getTrueArgs());
  auto FalseArgs = getOpValueArray<8>(Inst->getFalseArgs());
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
  doPostProcess(Inst,
       getBuilder().createCheckedCastBranch(getOpLocation(Inst->getLoc()),
                                            Inst->getCastKind(),
                                            getOpValue(Inst->getOperand()),
                                            getOpType(Inst->getCastType()),
                                            OpSuccBB, OpFailBB));
}

template<typename ImplClass>
void SILCloner<ImplClass>::visitCheckedCastAddrBranchInst(
                                             CheckedCastAddrBranchInst *Inst) {
  SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
  SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
  SILValue SrcValue = getOpValue(Inst->getSrc());
  SILValue DestValue = getOpValue(Inst->getDest());
  doPostProcess(Inst,
       getBuilder().createCheckedCastAddrBranch(getOpLocation(Inst->getLoc()),
                                                Inst->getCastKind(),
                                                SrcValue, DestValue,
                                                OpSuccBB, OpFailBB));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitSwitchIntInst(SwitchIntInst *Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<APInt, SILBasicBlock*>, 8> CaseBBs;
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  doPostProcess(Inst,
    getBuilder().createSwitchInt(getOpLocation(Inst->getLoc()),
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
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
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
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  doPostProcess(Inst,
    getBuilder().createSwitchEnumAddr(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      DefaultBB, CaseBBs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitDynamicMethodBranchInst(DynamicMethodBranchInst *Inst) {
  SILBasicBlock *OpHasMethodBB = getOpBasicBlock(Inst->getHasMethodBB());
  SILBasicBlock *OpHasNoMethodBB = getOpBasicBlock(Inst->getNoMethodBB());
  doPostProcess(Inst,
    getBuilder().createDynamicMethodBranch(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand()),
                                           Inst->getMember(),
                                           OpHasMethodBB, OpHasNoMethodBB));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::
visitProjectBlockStorageInst(ProjectBlockStorageInst *I) {
  doPostProcess(I, getBuilder().createProjectBlockStorage(
                                                getOpLocation(I->getLoc()),
                                                getOpValue(I->getOperand()),
                                                getOpType(I->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::
visitInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *I) {
  doPostProcess(I, getBuilder().createInitBlockStorageHeader(
                                         getOpLocation(I->getLoc()),
                                         getOpValue(I->getBlockStorage()),
                                         getOpValue(I->getInvokeFunction()),
                                         getOpType(I->getType())));
}

} // end namespace swift

#endif
