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
class SILCloner : protected SILVisitor<SILCloner<ImplClass>, SILValue> {
  friend class SILVisitor<SILCloner<ImplClass>, SILValue>;

public:
  explicit SILCloner(SILFunction &F) : Builder(F) { }

#define VALUE(CLASS, PARENT) \
  SILValue visit##CLASS(CLASS *I) {                                   \
    return getOpValue(I);                                             \
  }
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
  SILValue visit##CLASS(CLASS *I);
#include "swift/SIL/SILNodes.def"

protected:
  SILBuilder &getBuilder() { return Builder; }

  // Derived classes of SILCloner using the CRTP can implement the following
  // functions to customize behavior; the remap functions are called before
  // cloning to modify constructor arguments and the post process function is
  // called afterwards on the result.
  SILLocation remapLocation(SILLocation Loc) { return Loc; }
  SILType remapType(SILType Ty) { return Ty; }
  SILValue remapValue(SILValue Value) { return Value; }
  SILFunction *remapFunction(SILFunction *Func) { return Func; }
  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }
  SILValue postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    return Cloned;
  }

private:
  SILLocation getOpLocation(SILLocation Loc) {
    return static_cast<ImplClass*>(this)->remapLocation(Loc);
  }
  SILType getOpType(SILType Ty) {
    return static_cast<ImplClass*>(this)->remapType(Ty);
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
  SILValue doPostProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    return static_cast<ImplClass*>(this)->postProcess(Orig, Cloned);
  }

  SILBuilder Builder;
};

/// SILInstructionCloner - Concrete SILCloner subclass which can only be called
/// directly on instructions and clones them without any remapping of locations,
/// types, values, etc.
class SILInstructionCloner : public SILCloner<SILInstructionCloner> {
public:
  SILInstructionCloner(SILFunction &F)
    : SILCloner<SILInstructionCloner>(F) { }

#define INST(CLASS, PARENT, MEMBEHAVIOR)                              \
  CLASS *clone(CLASS *I) {                                            \
    SILValue clone = SILCloner<SILInstructionCloner>::visit##CLASS(I);\
    assert(clone->getKind() == I->getKind() &&                        \
           clone.getResultNumber() == 0);                             \
    return static_cast<CLASS *>(clone.getDef());                      \
  }
#include "swift/SIL/SILNodes.def"

  SILInstruction *clone(SILInstruction *I) {
    SILValue clone = SILCloner<SILInstructionCloner>::visit(I);
    assert(clone->getKind() == I->getKind() &&
           clone.getResultNumber() == 0);
    return static_cast<SILInstruction *>(clone.getDef());
  }
};

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAllocStackInst(AllocStackInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAllocStack(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAllocRefInst(AllocRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAllocRef(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAllocBoxInst(AllocBoxInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAllocBox(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getElementType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAllocArrayInst(AllocArrayInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAllocArray(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType()),
                             getOpValue(Inst->getNumElements())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitApplyInst(ApplyInst* Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  return doPostProcess(Inst,
    Builder.createApply(getOpLocation(Inst->getLoc()),
                        getOpValue(Inst->getCallee()),
                        getOpType(Inst->getType()), Args));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitPartialApplyInst(PartialApplyInst* Inst) {
  auto Args = getOpValueArray<8>(Inst->getArguments());
  return doPostProcess(Inst,
    Builder.createPartialApply(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getCallee()), Args,
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitBuiltinFunctionRefInst(BuiltinFunctionRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createBuiltinFunctionRef(getOpLocation(Inst->getLoc()),
                                     Inst->getFunction(),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitFunctionRefInst(FunctionRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                              getOpFunction(Inst->getFunction())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitGlobalAddrInst(GlobalAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createGlobalAddr(getOpLocation(Inst->getLoc()), Inst->getGlobal(),
                             getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitIntegerLiteralInst(IntegerLiteralInst* Inst) {
  return doPostProcess(Inst,
    Builder.createIntegerLiteral(getOpLocation(Inst->getLoc()),
                                 getOpType(Inst->getType()), Inst->getValue()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitFloatLiteralInst(FloatLiteralInst* Inst) {
  return doPostProcess(Inst,
    Builder.createFloatLiteral(getOpLocation(Inst->getLoc()),
                               getOpType(Inst->getType()), Inst->getValue()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStringLiteralInst(StringLiteralInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStringLiteral(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType()), Inst->getValue()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitLoadInst(LoadInst* Inst) {
  return doPostProcess(Inst,
    Builder.createLoad(getOpLocation(Inst->getLoc()),
                       getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStoreInst(StoreInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStore(getOpLocation(Inst->getLoc()),
                        getOpValue(Inst->getSrc()),
                        getOpValue(Inst->getDest())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAssignInst(AssignInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAssign(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getSrc()),
                         getOpValue(Inst->getDest())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitLoadWeakInst(LoadWeakInst* Inst) {
  return doPostProcess(Inst,
    Builder.createLoadWeak(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getOperand()),
                           Inst->isTake()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStoreWeakInst(StoreWeakInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStoreWeak(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getSrc()),
                            getOpValue(Inst->getDest()),
                            Inst->isInitializationOfDest()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitInitializeVarInst(InitializeVarInst* Inst) {
  return doPostProcess(Inst,
    Builder.createInitializeVar(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->canDefaultConstruct()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitCopyAddrInst(CopyAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createCopyAddr(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getSrc()),
                           getOpValue(Inst->getDest()),
                           Inst->isTakeOfSrc(),
                           Inst->isInitializationOfDest()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitSpecializeInst(SpecializeInst* Inst) {
  return doPostProcess(Inst,
    Builder.createSpecialize(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getOperand()),
                             Inst->getSubstitutions(), Inst->getType()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitConvertFunctionInst(ConvertFunctionInst* Inst) {
  return doPostProcess(Inst,
    Builder.createConvertFunction(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitCoerceInst(CoerceInst* Inst) {
  return doPostProcess(Inst,
    Builder.createCoerce(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand()),
                         getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUpcastInst(UpcastInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUpcast(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand()),
                         getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAddressToPointerInst(AddressToPointerInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAddressToPointer(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitPointerToAddressInst(PointerToAddressInst* Inst) {
  return doPostProcess(Inst,
    Builder.createPointerToAddress(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitRefToObjectPointerInst(RefToObjectPointerInst* Inst) {
  return doPostProcess(Inst,
    Builder.createRefToObjectPointer(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitObjectPointerToRefInst(ObjectPointerToRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createObjectPointerToRef(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitRefToRawPointerInst(RefToRawPointerInst* Inst) {
  return doPostProcess(Inst,
    Builder.createRefToRawPointer(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitRawPointerToRefInst(RawPointerToRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createRawPointerToRef(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitRefToUnownedInst(RefToUnownedInst* Inst) {
  return doPostProcess(Inst,
    Builder.createRefToUnowned(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand()),
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnownedToRefInst(UnownedToRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnownedToRef(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand()),
                               getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitThinToThickFunctionInst(ThinToThickFunctionInst* Inst) {
  return doPostProcess(Inst,
    Builder.createThinToThickFunction(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitConvertCCInst(ConvertCCInst* Inst) {
  return doPostProcess(Inst,
    Builder.createConvertCC(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitBridgeToBlockInst(BridgeToBlockInst* Inst) {
  return doPostProcess(Inst,
    Builder.createBridgeToBlock(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitArchetypeRefToSuperInst(ArchetypeRefToSuperInst* Inst) {
  return doPostProcess(Inst,
    Builder.createArchetypeRefToSuper(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitIsNonnullInst(IsNonnullInst* Inst) {
  return doPostProcess(Inst,
    Builder.createIsNonnull(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDowncastInst(DowncastInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDowncast(getOpLocation(Inst->getLoc()),
                           getOpValue(Inst->getOperand()),
                           getOpType(Inst->getType()), Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitSuperToArchetypeRefInst(SuperToArchetypeRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createSuperToArchetypeRef(getOpLocation(Inst->getLoc()),
                                      getOpValue(Inst->getOperand()),
                                      getOpType(Inst->getType()),
                                      Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDowncastArchetypeAddrInst(DowncastArchetypeAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDowncastArchetypeAddr(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand()),
                                        getOpType(Inst->getType()),
                                        Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDowncastArchetypeRefInst(DowncastArchetypeRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDowncastArchetypeRef(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType()),
                                       Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitProjectDowncastExistentialAddrInst(ProjectDowncastExistentialAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createProjectDowncastExistentialAddr(getOpLocation(Inst->getLoc()),
                                                 getOpValue(Inst->getOperand()),
                                                 getOpType(Inst->getType()),
                                                 Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDowncastExistentialRefInst(DowncastExistentialRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDowncastExistentialRef(getOpLocation(Inst->getLoc()),
                                         getOpValue(Inst->getOperand()),
                                         getOpType(Inst->getType()),
                                         Inst->getMode()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStructInst(StructInst* Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  return doPostProcess(Inst,
    Builder.createStruct(getOpLocation(Inst->getLoc()),
                         getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitTupleInst(TupleInst* Inst) {
  auto Elements = getOpValueArray<8>(Inst->getElements());
  return doPostProcess(Inst,
    Builder.createTuple(getOpLocation(Inst->getLoc()),
                        getOpType(Inst->getType()), Elements));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnionInst(UnionInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnion(getOpLocation(Inst->getLoc()),
                        Inst->hasOperand() ? getOpValue(Inst->getOperand())
                                           : SILValue(),
                        Inst->getElement(),
                        getOpType(Inst->getType())));
}
  
template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnionDataAddrInst(UnionDataAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnionDataAddr(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getElement(),
                                getOpType(Inst->getType())));
}
  
template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitInjectUnionAddrInst(InjectUnionAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createInjectUnionAddr(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  Inst->getElement()));
}
  
template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitBuiltinZeroInst(BuiltinZeroInst* Inst) {
  return doPostProcess(Inst,
    Builder.createBuiltinZero(getOpLocation(Inst->getLoc()),
                              getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitMetatypeInst(MetatypeInst* Inst) {
  return doPostProcess(Inst,
    Builder.createMetatype(getOpLocation(Inst->getLoc()),
                           getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitClassMetatypeInst(ClassMetatypeInst* Inst) {
  return doPostProcess(Inst,
    Builder.createClassMetatype(getOpLocation(Inst->getLoc()),
                                getOpType(Inst->getType()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitArchetypeMetatypeInst(ArchetypeMetatypeInst* Inst) {
  return doPostProcess(Inst,
    Builder.createArchetypeMetatype(getOpLocation(Inst->getLoc()),
                                    getOpType(Inst->getType()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitProtocolMetatypeInst(ProtocolMetatypeInst* Inst) {
  return doPostProcess(Inst,
    Builder.createProtocolMetatype(getOpLocation(Inst->getLoc()),
                                   getOpType(Inst->getType()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitModuleInst(ModuleInst* Inst) {
  return doPostProcess(Inst,
    Builder.createModule(getOpLocation(Inst->getLoc()),
                         getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitTupleExtractInst(TupleExtractInst* Inst) {
  return doPostProcess(Inst,
    Builder.createTupleExtractInst(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getFieldNo(),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitTupleElementAddrInst(TupleElementAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createTupleElementAddr(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand()),
                                   Inst->getFieldNo(),
                                   getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStructExtractInst(StructExtractInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStructExtract(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand()),
                                Inst->getField(),
                                getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStructElementAddrInst(StructElementAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStructElementAddr(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand()),
                                    Inst->getField(),
                                    getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitRefElementAddrInst(RefElementAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createRefElementAddr(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand()),
                                 Inst->getField(),
                                 getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitClassMethodInst(ClassMethodInst* Inst) {
  return doPostProcess(Inst,
    Builder.createClassMethod(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              Inst->getMember(),
                              getOpType(Inst->getType()),
                              Inst->isVolatile()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitSuperMethodInst(SuperMethodInst* Inst) {
  return doPostProcess(Inst,
    Builder.createSuperMethod(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand()),
                              Inst->getMember(),
                              getOpType(Inst->getType()),
                              Inst->isVolatile()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitArchetypeMethodInst(ArchetypeMethodInst* Inst) {
  return doPostProcess(Inst,
    Builder.createArchetypeMethod(getOpLocation(Inst->getLoc()),
                                  getOpType(Inst->getLookupArchetype()),
                                  Inst->getMember(),
                                  getOpType(Inst->getType()),
                                  Inst->isVolatile()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitProtocolMethodInst(ProtocolMethodInst* Inst) {
  return doPostProcess(Inst,
    Builder.createProtocolMethod(getOpLocation(Inst->getLoc()),
                                 getOpValue(Inst->getOperand()),
                                 Inst->getMember(),
                                 getOpType(Inst->getType()),
                                 Inst->isVolatile()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitProjectExistentialInst(ProjectExistentialInst* Inst) {
  return doPostProcess(Inst,
    Builder.createProjectExistential(getOpLocation(Inst->getLoc()),
                                     getOpValue(Inst->getOperand()),
                                     getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitProjectExistentialRefInst(ProjectExistentialRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createProjectExistentialRef(getOpLocation(Inst->getLoc()),
                                        getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitInitExistentialInst(InitExistentialInst* Inst) {
  return doPostProcess(Inst,
    Builder.createInitExistential(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getOperand()),
                                  getOpType(Inst->getConcreteType()),
                                  Inst->getConformances()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitInitExistentialRefInst(InitExistentialRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createInitExistentialRef(getOpLocation(Inst->getLoc()),
                                     getOpType(Inst->getType()),
                                     getOpValue(Inst->getOperand()),
                                     Inst->getConformances()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDeinitExistentialInst(DeinitExistentialInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDeinitExistential(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUpcastExistentialInst(UpcastExistentialInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUpcastExistential(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getSrcExistential()),
                                    getOpValue(Inst->getDestExistential()),
                                    (IsTake_t)Inst->isTakeOfSrc()));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUpcastExistentialRefInst(UpcastExistentialRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUpcastExistentialRef(getOpLocation(Inst->getLoc()),
                                       getOpValue(Inst->getOperand()),
                                       getOpType(Inst->getType())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStrongRetainInst(StrongRetainInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStrongRetainInst(getOpLocation(Inst->getLoc()),
                                   getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::
visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStrongRetainAutoreleased(getOpLocation(Inst->getLoc()),
                                           getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitStrongReleaseInst(StrongReleaseInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStrongReleaseInst(getOpLocation(Inst->getLoc()),
                                    getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::
visitStrongRetainUnownedInst(StrongRetainUnownedInst* Inst) {
  return doPostProcess(Inst,
    Builder.createStrongRetainUnowned(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnownedRetainInst(UnownedRetainInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnownedRetain(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnownedReleaseInst(UnownedReleaseInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnownedRelease(getOpLocation(Inst->getLoc()),
                                getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDeallocStackInst(DeallocStackInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDeallocStack(getOpLocation(Inst->getLoc()),
                               getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDeallocRefInst(DeallocRefInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDeallocRef(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDeallocBoxInst(DeallocBoxInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDeallocBox(getOpLocation(Inst->getLoc()),
                             getOpType(Inst->getElementType()),
                             getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDestroyAddrInst(DestroyAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createDestroyAddr(getOpLocation(Inst->getLoc()),
                              getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitIndexAddrInst(IndexAddrInst* Inst) {
  return doPostProcess(Inst,
    Builder.createIndexAddr(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getBase()),
                            getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitIndexRawPointerInst(IndexRawPointerInst* Inst) {
  return doPostProcess(Inst,
    Builder.createIndexRawPointer(getOpLocation(Inst->getLoc()),
                                  getOpValue(Inst->getBase()),
                                  getOpValue(Inst->getIndex())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitUnreachableInst(UnreachableInst* Inst) {
  return doPostProcess(Inst,
    Builder.createUnreachable(getOpLocation(Inst->getLoc())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitReturnInst(ReturnInst* Inst) {
  return doPostProcess(Inst,
    Builder.createReturn(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitAutoreleaseReturnInst(AutoreleaseReturnInst* Inst) {
  return doPostProcess(Inst,
    Builder.createAutoreleaseReturn(getOpLocation(Inst->getLoc()),
                         getOpValue(Inst->getOperand())));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitBranchInst(BranchInst* Inst) {
  auto Args = getOpValueArray<8>(Inst->getArgs());
  return doPostProcess(Inst,
    Builder.createBranch(getOpLocation(Inst->getLoc()),
                         getOpBasicBlock(Inst->getDestBB()), Args));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitCondBranchInst(CondBranchInst* Inst) {
  auto TrueArgs = getOpValueArray<8>(Inst->getTrueArgs());
  auto FalseArgs = getOpValueArray<8>(Inst->getFalseArgs());
  return doPostProcess(Inst,
    Builder.createCondBranch(getOpLocation(Inst->getLoc()),
                             getOpValue(Inst->getCondition()),
                             getOpBasicBlock(Inst->getTrueBB()), TrueArgs,
                             getOpBasicBlock(Inst->getFalseBB()), FalseArgs));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitSwitchIntInst(SwitchIntInst* Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<APInt, SILBasicBlock*>, 8> CaseBBs;
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  return doPostProcess(Inst,
    Builder.createSwitchInt(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            DefaultBB, CaseBBs));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitSwitchUnionInst(SwitchUnionInst* Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<UnionElementDecl*, SILBasicBlock*>, 8> CaseBBs;
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  return doPostProcess(Inst,
    Builder.createSwitchUnion(getOpLocation(Inst->getLoc()),
                            getOpValue(Inst->getOperand()),
                            DefaultBB, CaseBBs));
}

template<typename ImplClass>
SILValue
SILCloner<ImplClass>::visitDestructiveSwitchUnionAddrInst(
                                        DestructiveSwitchUnionAddrInst* Inst) {
  SILBasicBlock *DefaultBB = nullptr;
  if (Inst->hasDefault())
    DefaultBB = getOpBasicBlock(Inst->getDefaultBB());
  SmallVector<std::pair<UnionElementDecl*, SILBasicBlock*>, 8> CaseBBs;
  for(int i = 0, e = Inst->getNumCases(); i != e; ++i)
    CaseBBs.push_back(std::make_pair(Inst->getCase(i).first,
                                     getOpBasicBlock(Inst->getCase(i).second)));
  return doPostProcess(Inst,
    Builder.createDestructiveSwitchUnionAddr(getOpLocation(Inst->getLoc()),
                                             getOpValue(Inst->getOperand()),
                                             DefaultBB, CaseBBs));
}
  
} // end namespace swift

#endif
