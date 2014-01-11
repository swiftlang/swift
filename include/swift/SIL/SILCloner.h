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
    return static_cast<ImplClass*>(this)->remapLocation(Loc);
  }
  SILType getOpType(SILType Ty) {
    return static_cast<ImplClass*>(this)->remapType(Ty);
  }
  ProtocolConformance *getOpConformance(SILType Ty,
                                        ProtocolConformance *Conformance) {
    return static_cast<ImplClass*>(this)->remapConformance(Ty, Conformance);
  }
  SILValue getOpValue(SILValue Value) {
    return static_cast<ImplClass*>(this)->remapValue(Value);
  }
  template <size_t N, typename ArrayRefType>
  SmallVector<SILValue, N> getOpValueArray(ArrayRefType Values) {
    SmallVector<SILValue, N> Ret(Values.size());
    for (unsigned i = 0, e = Values.size(); i != e; ++i)
      Ret[i] = static_cast<ImplClass*>(this)->remapValue(Values[i]);
    return Ret;
  }
  SILFunction *getOpFunction(SILFunction *Func) {
    return static_cast<ImplClass*>(this)->remapFunction(Func);
  }
  SILBasicBlock *getOpBasicBlock(SILBasicBlock *BB) {
    return static_cast<ImplClass*>(this)->remapBasicBlock(BB);
  }
  void doPostProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    static_cast<ImplClass*>(this)->postProcess(Orig, Cloned);
  }

  SILBuilder Builder;
  SILBasicBlock *InsertBeforeBB;
  llvm::DenseMap<ValueBase*, SILValue> ValueMap;
  llvm::DenseMap<SILInstruction*, SILInstruction*> InstructionMap;
  llvm::DenseMap<SILBasicBlock*, SILBasicBlock*> BBMap;
};

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::remapValue(SILValue Value) {
  auto VI = ValueMap.find(Value.getDef());
  if (VI != ValueMap.end()) {
    assert(Value.getResultNumber() == 0 &&
           "Non-zero result number of mapped value used?");
    return VI->second;
  }

  if (SILInstruction* I = dyn_cast<SILInstruction>(Value.getDef())) {
    auto II = InstructionMap.find(I);
    if (II != InstructionMap.end())
      return SILValue(II->second, Value.getResultNumber());
    llvm_unreachable("Unmapped instruction while cloning?");
  }

  // If we have undef, just copy it.
  if (SILUndef *U = dyn_cast<SILUndef>(Value.getDef()))
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
    static_cast<ImplClass*>(this)->visit(I);
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
    Builder.createAllocStack(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocRefInst(AllocRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createAllocRef(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getType()),
                           Inst->isObjC()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocBoxInst(AllocBoxInst *Inst) {
  doPostProcess(Inst,
    Builder.createAllocBox(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getElementType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAllocArrayInst(AllocArrayInst *Inst) {
  doPostProcess(Inst,
    Builder.createAllocArray(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType()),
                             getOpValue(Inst->getNumElements())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitApplyInst(ApplyInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  doPostProcess(Inst,
    Builder.createApply(getOpLocation(Inst->getLoc()),
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
    Builder.createPartialApply(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getCallee()),
                               getOpType(Inst->getSubstCalleeSILType()),
                               Inst->getSubstitutions(), Args,
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *Inst){
  doPostProcess(Inst,
    Builder.createBuiltinFunctionRef(getOpLocation(Inst->getLoc()),
                                     Inst->getName(),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFunctionRefInst(FunctionRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                              getOpFunction(Inst->getReferencedFunction())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitGlobalAddrInst(GlobalAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createGlobalAddr(getOpLocation(Inst->getLoc()), Inst->getGlobal(),
                             getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSILGlobalAddrInst(SILGlobalAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createSILGlobalAddr(getOpLocation(Inst->getLoc()),
                                Inst->getReferencedGlobal()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIntegerLiteralInst(IntegerLiteralInst *Inst) {
  doPostProcess(Inst,
    Builder.createIntegerLiteral(getOpLocation(Inst->getLoc()),
                                 getOpType(Inst->getType()), Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitFloatLiteralInst(FloatLiteralInst *Inst) {
  doPostProcess(Inst,
    Builder.createFloatLiteral(getOpLocation(Inst->getLoc()),
                               getOpType(Inst->getType()), Inst->getValue()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStringLiteralInst(StringLiteralInst *Inst) {
  doPostProcess(Inst,
                Builder.createStringLiteral(getOpLocation(Inst->getLoc()),
                                            Inst->getValue(),
                                            Inst->getEncoding()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadInst(LoadInst *Inst) {
  doPostProcess(Inst,
    Builder.createLoad(getOpLocation(Inst->getLoc()),
                       getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreInst(StoreInst *Inst) {
  doPostProcess(Inst,
    Builder.createStore(getOpLocation(Inst->getLoc()),
                        getOpValue(Inst->getSrc()),
                        getOpValue(Inst->getDest())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAssignInst(AssignInst *Inst) {
  doPostProcess(Inst,
    Builder.createAssign(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getSrc()),
                         getOpValue(Inst->getDest())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkUninitializedInst(MarkUninitializedInst *Inst) {
  doPostProcess(Inst,
             Builder.createMarkUninitialized(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             Inst->getKind()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *Inst){
  auto Elements = getOpValueArray<8>(Inst->getElements());
  doPostProcess(Inst,
               Builder.createMarkFunctionEscape(getOpLocation(Inst->getLoc()),
                                                Elements));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueInst(DebugValueInst *Inst) {
  doPostProcess(Inst,
                Builder.createDebugValue(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand())));
}
template<typename ImplClass>
void
SILCloner<ImplClass>::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  doPostProcess(Inst,
                Builder.createDebugValueAddr(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand())));
}


template<typename ImplClass>
void
SILCloner<ImplClass>::visitLoadWeakInst(LoadWeakInst *Inst) {
  doPostProcess(Inst,
    Builder.createLoadWeak(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getOperand()),
                           Inst->isTake()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStoreWeakInst(StoreWeakInst *Inst) {
  doPostProcess(Inst,
    Builder.createStoreWeak(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getSrc()),
                            getOpValue(Inst->getDest()),
                            Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyAddrInst(CopyAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createCopyAddr(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getSrc()),
                           getOpValue(Inst->getDest()),
                           Inst->isTakeOfSrc(),
                           Inst->isInitializationOfDest()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitConvertFunctionInst(ConvertFunctionInst *Inst) {
  doPostProcess(Inst,
    Builder.createConvertFunction(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCoerceInst(CoerceInst *Inst) {
  doPostProcess(Inst,
    Builder.createCoerce(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand()),
                         getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastInst(UpcastInst *Inst) {
  doPostProcess(Inst,
    Builder.createUpcast(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand()),
                         getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAddressToPointerInst(AddressToPointerInst *Inst) {
  doPostProcess(Inst,
    Builder.createAddressToPointer(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPointerToAddressInst(PointerToAddressInst *Inst) {
  doPostProcess(Inst,
    Builder.createPointerToAddress(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToObjectPointerInst(RefToObjectPointerInst *Inst) {
  doPostProcess(Inst,
    Builder.createRefToObjectPointer(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitObjectPointerToRefInst(ObjectPointerToRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createObjectPointerToRef(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToRawPointerInst(RefToRawPointerInst *Inst) {
  doPostProcess(Inst,
    Builder.createRefToRawPointer(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRawPointerToRefInst(RawPointerToRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createRawPointerToRef(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefToUnownedInst(RefToUnownedInst *Inst) {
  doPostProcess(Inst,
    Builder.createRefToUnowned(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand()),
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedToRefInst(UnownedToRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createUnownedToRef(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand()),
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitThinToThickFunctionInst(ThinToThickFunctionInst *Inst) {
  doPostProcess(Inst,
    Builder.createThinToThickFunction(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBridgeToBlockInst(BridgeToBlockInst *Inst) {
  doPostProcess(Inst,
    Builder.createBridgeToBlock(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitArchetypeRefToSuperInst(ArchetypeRefToSuperInst *Inst) {
  doPostProcess(Inst,
    Builder.createArchetypeRefToSuper(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIsNonnullInst(IsNonnullInst *Inst) {
  doPostProcess(Inst,
    Builder.createIsNonnull(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnconditionalCheckedCastInst(
                                          UnconditionalCheckedCastInst *Inst) {
  doPostProcess(Inst,
         Builder.createUnconditionalCheckedCast(getOpLocation(Inst->getLoc()),
                                                Inst->getCastKind(),
                                                getOpValue(Inst->getOperand()),
                                                getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitCopyValueInst(CopyValueInst *Inst) {
  doPostProcess(Inst,
    Builder.createCopyValue(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDestroyValueInst(DestroyValueInst *Inst) {
  doPostProcess(Inst,
    Builder.createDestroyValue(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructInst(StructInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  doPostProcess(Inst,
    Builder.createStruct(getOpLocation(Inst->getLoc()),
                         getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleInst(TupleInst *Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  doPostProcess(Inst,
    Builder.createTuple(getOpLocation(Inst->getLoc()),
                        getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitEnumInst(EnumInst *Inst) {
  doPostProcess(Inst,
    Builder.createEnum(getOpLocation(Inst->getLoc()),
                        Inst->hasOperand() ? getOpValue(Inst->getOperand())
                                           : SILValue(),
                        Inst->getElement(),
                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitEnumDataAddrInst(InitEnumDataAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createInitEnumDataAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getElement(),
                                getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitTakeEnumDataAddrInst(TakeEnumDataAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createTakeEnumDataAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getElement(),
                                getOpType(Inst->getType())));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitInjectEnumAddrInst(InjectEnumAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createInjectEnumAddr(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  Inst->getElement()));
}
  
template<typename ImplClass>
void
SILCloner<ImplClass>::visitMetatypeInst(MetatypeInst *Inst) {
  doPostProcess(Inst,
    Builder.createMetatype(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitClassMetatypeInst(ClassMetatypeInst *Inst) {
  doPostProcess(Inst,
    Builder.createClassMetatype(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitArchetypeMetatypeInst(ArchetypeMetatypeInst *Inst) {
  doPostProcess(Inst,
    Builder.createArchetypeMetatype(getOpLocation(Inst->getLoc()),
                                    getOpType(Inst->getType()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitProtocolMetatypeInst(ProtocolMetatypeInst *Inst) {
  doPostProcess(Inst,
    Builder.createProtocolMetatype(getOpLocation(Inst->getLoc()),
                                   getOpType(Inst->getType()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleExtractInst(TupleExtractInst *Inst) {
  doPostProcess(Inst,
    Builder.createTupleExtract(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand()),
                               Inst->getFieldNo(),
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitTupleElementAddrInst(TupleElementAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createTupleElementAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getFieldNo(),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructExtractInst(StructExtractInst *Inst) {
  doPostProcess(Inst,
    Builder.createStructExtract(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getField(),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStructElementAddrInst(StructElementAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createStructElementAddr(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getField(),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitRefElementAddrInst(RefElementAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createRefElementAddr(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand()),
                                 Inst->getField(),
                                 getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitClassMethodInst(ClassMethodInst *Inst) {
  doPostProcess(Inst,
    Builder.createClassMethod(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              Inst->getMember(),
                              getOpType(Inst->getType()),
                              Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSuperMethodInst(SuperMethodInst *Inst) {
  doPostProcess(Inst,
    Builder.createSuperMethod(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              Inst->getMember(),
                              getOpType(Inst->getType()),
                              Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitPeerMethodInst(PeerMethodInst *Inst) {
  doPostProcess(Inst,
                Builder.createPeerMethod(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         Inst->getMember(),
                                         getOpType(Inst->getType()),
                                         Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitArchetypeMethodInst(ArchetypeMethodInst *Inst) {
  doPostProcess(Inst,
    Builder.createArchetypeMethod(getOpLocation(Inst->getLoc()),
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
    Builder.createProtocolMethod(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand()),
                                 Inst->getMember(),
                                 getOpType(Inst->getType()),
                                 Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDynamicMethodInst(DynamicMethodInst *Inst) {
  doPostProcess(Inst,
    Builder.createDynamicMethod(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getMember(),
                                getOpType(Inst->getType()),
                                Inst->isVolatile()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitProjectExistentialInst(ProjectExistentialInst *Inst) {
  doPostProcess(Inst,
    Builder.createProjectExistential(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitProjectExistentialRefInst(ProjectExistentialRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createProjectExistentialRef(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitExistentialInst(InitExistentialInst *Inst) {
  doPostProcess(Inst,
    Builder.createInitExistential(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getConcreteType()),
                                  Inst->getConformances()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitInitExistentialRefInst(InitExistentialRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createInitExistentialRef(getOpLocation(Inst->getLoc()),
                                     getOpType(Inst->getType()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getConformances()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeinitExistentialInst(DeinitExistentialInst *Inst) {
  doPostProcess(Inst,
    Builder.createDeinitExistential(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastExistentialInst(UpcastExistentialInst *Inst) {
  doPostProcess(Inst,
    Builder.createUpcastExistential(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getSrcExistential()),
                                    getOpValue(Inst->getDestExistential()),
                                    (IsTake_t)Inst->isTakeOfSrc()));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUpcastExistentialRefInst(UpcastExistentialRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createUpcastExistentialRef(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongRetainInst(StrongRetainInst *Inst) {
  doPostProcess(Inst,
    Builder.createStrongRetain(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *Inst) {
  doPostProcess(Inst,
    Builder.createStrongRetainAutoreleased(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  doPostProcess(Inst,
    Builder.createStrongRelease(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::
visitStrongRetainUnownedInst(StrongRetainUnownedInst *Inst) {
  doPostProcess(Inst,
    Builder.createStrongRetainUnowned(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedRetainInst(UnownedRetainInst *Inst) {
  doPostProcess(Inst,
    Builder.createUnownedRetain(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnownedReleaseInst(UnownedReleaseInst *Inst) {
  doPostProcess(Inst,
    Builder.createUnownedRelease(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocStackInst(DeallocStackInst *Inst) {
  doPostProcess(Inst,
    Builder.createDeallocStack(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocRefInst(DeallocRefInst *Inst) {
  doPostProcess(Inst,
    Builder.createDeallocRef(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDeallocBoxInst(DeallocBoxInst *Inst) {
  doPostProcess(Inst,
    Builder.createDeallocBox(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType()),
                             getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDestroyAddrInst(DestroyAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createDestroyAddr(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondFailInst(CondFailInst *Inst) {
  doPostProcess(Inst,
    Builder.createCondFail(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexAddrInst(IndexAddrInst *Inst) {
  doPostProcess(Inst,
    Builder.createIndexAddr(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getBase()),
                            getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitIndexRawPointerInst(IndexRawPointerInst *Inst) {
  doPostProcess(Inst,
    Builder.createIndexRawPointer(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getBase()),
                                  getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitUnreachableInst(UnreachableInst *Inst) {
  doPostProcess(Inst,
    Builder.createUnreachable(getOpLocation(Inst->getLoc())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitReturnInst(ReturnInst *Inst) {
  doPostProcess(Inst,
    Builder.createReturn(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitAutoreleaseReturnInst(AutoreleaseReturnInst *Inst) {
  doPostProcess(Inst,
    Builder.createAutoreleaseReturn(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitBranchInst(BranchInst *Inst) {
  auto Args = getOpValueArray<8>(Inst->getArgs());
  doPostProcess(Inst,
    Builder.createBranch(getOpLocation(Inst->getLoc()),
                         getOpBasicBlock(Inst->getDestBB()), Args));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCondBranchInst(CondBranchInst *Inst) {
  auto TrueArgs = getOpValueArray<8>(Inst->getTrueArgs());
  auto FalseArgs = getOpValueArray<8>(Inst->getFalseArgs());
  doPostProcess(Inst,
    Builder.createCondBranch(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getCondition()),
                             getOpBasicBlock(Inst->getTrueBB()), TrueArgs,
                             getOpBasicBlock(Inst->getFalseBB()), FalseArgs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  doPostProcess(Inst,
       Builder.createCheckedCastBranch(getOpLocation(Inst->getLoc()),
                                       Inst->getCastKind(),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getCastType()),
                                       getOpBasicBlock(Inst->getSuccessBB()),
                                       getOpBasicBlock(Inst->getFailureBB())));
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
    Builder.createSwitchInt(getOpLocation(Inst->getLoc()),
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
    Builder.createSwitchEnum(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            DefaultBB, CaseBBs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitSwitchEnumAddrInst(
                                        SwitchEnumAddrInst *Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> CaseBBs;
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  doPostProcess(Inst,
    Builder.createSwitchEnumAddr(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             DefaultBB, CaseBBs));
}

template<typename ImplClass>
void
SILCloner<ImplClass>::visitDynamicMethodBranchInst(
                        DynamicMethodBranchInst *Inst) {
  doPostProcess(Inst,
    Builder.createDynamicMethodBranch(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      Inst->getMember(),
                                      getOpBasicBlock(Inst->getHasMethodBB()),
                                      getOpBasicBlock(Inst->getNoMethodBB())));
}

} // end namespace swift

#endif
