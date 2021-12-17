//===--- SILBridgingUtils.cpp - Utilities for swift bridging --------------===//
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

#include "swift/SIL/SILNode.h"
#include "swift/SIL/SILBridgingUtils.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILBuilder.h"

#include <string>

using namespace swift;

namespace {

bool nodeMetatypesInitialized = false;

// Filled in by class registration in initializeLibSwift().
SwiftMetatype nodeMetatypes[(unsigned)SILNodeKind::Last_SILNode + 1];

}

static_assert(sizeof(BridgedLocation) == sizeof(SILDebugLocation),
              "BridgedLocation has wrong size");

/// Fills \p storage with all Values from the bridged \p values array.
ArrayRef<SILValue> swift::getSILValues(BridgedValueArray values,
                                       SmallVectorImpl<SILValue> &storage) {
  auto *base = reinterpret_cast<const SwiftObject *>(values.data);

  // The bridged array contains class existentials, which have a layout of two
  // words. The first word is the actual object. Pick the objects and store them
  // into storage.
  for (unsigned idx = 0; idx < values.count; ++idx) {
    storage.push_back(castToSILValue({base[idx * 2]}));
  }
  return storage;
}

//===----------------------------------------------------------------------===//
//                          Class registration
//===----------------------------------------------------------------------===//

static llvm::StringMap<SILNodeKind> valueNamesToKind;
static llvm::SmallPtrSet<SwiftMetatype, 4> unimplementedTypes;

// Utility to fill in a metatype of an "unimplemented" class for a whole range
// of class types.
static void setUnimplementedRange(SwiftMetatype metatype,
                                  SILNodeKind from, SILNodeKind to) {
  unimplementedTypes.insert(metatype);
  for (unsigned kind = (unsigned)from; kind <= (unsigned)to; ++kind) {
    assert((!nodeMetatypes[kind] || unimplementedTypes.count(metatype)) &&
           "unimplemented nodes must be registered first");
    nodeMetatypes[kind] = metatype;
  }
}

/// Registers the metatype of a libswift class.
/// Called by initializeLibSwift().
void registerBridgedClass(BridgedStringRef className, SwiftMetatype metatype) {
  nodeMetatypesInitialized = true;
}

//===----------------------------------------------------------------------===//
//                            Bridging C functions
//===----------------------------------------------------------------------===//

void OStream_write(BridgedOStream os, BridgedStringRef str) {
  static_cast<raw_ostream *>(os.streamAddr)->write((const char*)(str.data), str.length);
}

/// Frees a string which was allocated by getCopiedBridgedStringRef.
void freeBridgedStringRef(BridgedStringRef str) {
  llvm::MallocAllocator().Deallocate(str.data, str.length);
}

//===----------------------------------------------------------------------===//
//                                SILFunction
//===----------------------------------------------------------------------===//

BridgedStringRef SILFunction_getName(BridgedFunction function) {
  return getBridgedStringRef(castToFunction(function)->getName());
}

std::string SILFunction_debugDescription(BridgedFunction function) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToFunction(function)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

OptionalBridgedBasicBlock SILFunction_firstBlock(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  if (f->empty())
    return {nullptr};
  return {nullptr};
}

OptionalBridgedBasicBlock SILFunction_lastBlock(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  if (f->empty())
    return {nullptr};
  return {nullptr};
}

SwiftInt SILFunction_numIndirectResultArguments(BridgedFunction function) {
  return castToFunction(function)->getLoweredFunctionType()->
          getNumIndirectFormalResults();
}

SwiftInt SILFunction_getSelfArgumentIndex(BridgedFunction function) {
  CanSILFunctionType fTy = castToFunction(function)->getLoweredFunctionType();
  if (!fTy->hasSelfParam())
    return -1;
  return fTy->getNumParameters() + fTy->getNumIndirectFormalResults() - 1;
}

//===----------------------------------------------------------------------===//
//                               SILBasicBlock
//===----------------------------------------------------------------------===//

static_assert(BridgedSuccessorSize == sizeof(SILSuccessor),
              "wrong bridged SILSuccessor size");

OptionalBridgedBasicBlock SILBasicBlock_next(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  auto iter = std::next(b->getIterator());
  if (iter == b->getParent()->end())
    return {nullptr};
  return {nullptr};
}

OptionalBridgedBasicBlock SILBasicBlock_previous(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  auto iter = std::next(b->getReverseIterator());
  if (iter == b->getParent()->rend())
    return {nullptr};
  return {nullptr};
}

BridgedFunction SILBasicBlock_getFunction(BridgedBasicBlock block) {
  return {nullptr};
}

std::string SILBasicBlock_debugDescription(BridgedBasicBlock block) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToBasicBlock(block)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

OptionalBridgedInstruction SILBasicBlock_firstInst(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  if (b->empty())
    return {nullptr};
  return {nullptr};
}

OptionalBridgedInstruction SILBasicBlock_lastInst(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  if (b->empty())
    return {nullptr};
  return {nullptr};
}

SwiftInt SILBasicBlock_getNumArguments(BridgedBasicBlock block) {
  return castToBasicBlock(block)->getNumArguments();
}

BridgedArgument SILBasicBlock_getArgument(BridgedBasicBlock block, SwiftInt index) {
  return {nullptr};
}

OptionalBridgedSuccessor SILBasicBlock_getFirstPred(BridgedBasicBlock block) {
  return {nullptr};
}

static SILSuccessor *castToSuccessor(BridgedSuccessor succ) {
  return const_cast<SILSuccessor *>(static_cast<const SILSuccessor *>(succ.succ));
}

OptionalBridgedSuccessor SILSuccessor_getNext(BridgedSuccessor succ) {
  return {nullptr};
}

BridgedBasicBlock SILSuccessor_getTargetBlock(BridgedSuccessor succ) {
  return {nullptr};
}

BridgedInstruction SILSuccessor_getContainingInst(BridgedSuccessor succ) {
  return {nullptr};
}

//===----------------------------------------------------------------------===//
//                                SILArgument
//===----------------------------------------------------------------------===//

BridgedBasicBlock SILArgument_getParent(BridgedArgument argument) {
  return {nullptr};
}

//===----------------------------------------------------------------------===//
//                                SILValue
//===----------------------------------------------------------------------===//

static_assert(BridgedOperandSize == sizeof(Operand),
              "wrong bridged Operand size");

std::string SILNode_debugDescription(BridgedNode node) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToSILNode(node)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

static Operand *castToOperand(BridgedOperand operand) {
  return const_cast<Operand *>(static_cast<const Operand *>(operand.op));
}

BridgedValue Operand_getValue(BridgedOperand operand) {
  return {nullptr};
}

OptionalBridgedOperand Operand_nextUse(BridgedOperand operand) {
  return {nullptr};
}

BridgedInstruction Operand_getUser(BridgedOperand operand) {
  return {nullptr};
}

SwiftInt Operand_isTypeDependent(BridgedOperand operand) {
  return castToOperand(operand)->isTypeDependent() ? 1 : 0;
}

OptionalBridgedOperand SILValue_firstUse(BridgedValue value) {
  return {nullptr};
}

BridgedType SILValue_getType(BridgedValue value) {
  return {nullptr};
}

//===----------------------------------------------------------------------===//
//                            SILType
//===----------------------------------------------------------------------===//

SwiftInt SILType_isAddress(BridgedType type) {
  return castToSILType(type).isAddress();
}

SwiftInt SILType_isTrivial(BridgedType type, BridgedFunction function) {
  return castToSILType(type).isTrivial(*castToFunction(function));
}

//===----------------------------------------------------------------------===//
//                            SILGlobalVariable
//===----------------------------------------------------------------------===//

BridgedStringRef SILGlobalVariable_getName(BridgedGlobalVar global) {
  return getBridgedStringRef(castToGlobal(global)->getName());
}

std::string SILGlobalVariable_debugDescription(BridgedGlobalVar global) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToGlobal(global)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                               SILInstruction
//===----------------------------------------------------------------------===//

OptionalBridgedInstruction SILInstruction_next(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  auto iter = std::next(i->getIterator());
  if (iter == i->getParent()->end())
    return {nullptr};
  return {nullptr};
}

OptionalBridgedInstruction SILInstruction_previous(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  auto iter = std::next(i->getReverseIterator());
  if (iter == i->getParent()->rend())
    return {nullptr};
  return {nullptr};
}

BridgedBasicBlock SILInstruction_getParent(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  assert(!i->isStaticInitializerInst() &&
         "cannot get the parent of a static initializer instruction");
  return {nullptr};
}

BridgedArrayRef SILInstruction_getOperands(BridgedInstruction inst) {
  return {nullptr, 0};
}

void SILInstruction_setOperand(BridgedInstruction inst, SwiftInt index,
                               BridgedValue value) {
  castToInst(inst)->setOperand((unsigned)index, castToSILValue(value));
}

BridgedLocation SILInstruction_getLocation(BridgedInstruction inst) {
  SILDebugLocation loc = castToInst(inst)->getDebugLocation();
  return *reinterpret_cast<BridgedLocation *>(&loc);
}

BridgedMemoryBehavior SILInstruction_getMemBehavior(BridgedInstruction inst) {
  return (BridgedMemoryBehavior)castToInst(inst)->getMemoryBehavior();
}

BridgedInstruction MultiValueInstResult_getParent(BridgedMultiValueResult result) {
  return {nullptr};
}

SwiftInt MultipleValueInstruction_getNumResults(BridgedInstruction inst) {
  return castToInst<MultipleValueInstruction>(inst)->getNumResults();
}
BridgedMultiValueResult
MultipleValueInstruction_getResult(BridgedInstruction inst, SwiftInt index) {
  return {nullptr};
}

BridgedArrayRef TermInst_getSuccessors(BridgedInstruction term) {
  auto successors = castToInst<TermInst>(term)->getSuccessors();
  return {(const unsigned char *)successors.data(), successors.size()};
}

//===----------------------------------------------------------------------===//
//                            Instruction classes
//===----------------------------------------------------------------------===//

BridgedStringRef CondFailInst_getMessage(BridgedInstruction cfi) {
  return getBridgedStringRef(castToInst<CondFailInst>(cfi)->getMessage());
}

BridgedGlobalVar GlobalAccessInst_getGlobal(BridgedInstruction globalInst) {
  return {nullptr};
}

SwiftInt TupleExtractInst_fieldIndex(BridgedInstruction tei) {
  return castToInst<TupleExtractInst>(tei)->getFieldIndex();
}

SwiftInt TupleElementAddrInst_fieldIndex(BridgedInstruction teai) {
  return castToInst<TupleElementAddrInst>(teai)->getFieldIndex();
}

SwiftInt StructExtractInst_fieldIndex(BridgedInstruction sei) {
  return castToInst<StructExtractInst>(sei)->getFieldIndex();
}

SwiftInt StructElementAddrInst_fieldIndex(BridgedInstruction seai) {
  return castToInst<StructElementAddrInst>(seai)->getFieldIndex();
}

SwiftInt EnumInst_caseIndex(BridgedInstruction ei) {
  return getCaseIndex(castToInst<EnumInst>(ei)->getElement());
}

SwiftInt UncheckedEnumDataInst_caseIndex(BridgedInstruction uedi) {
  return getCaseIndex(castToInst<UncheckedEnumDataInst>(uedi)->getElement());
}

SwiftInt RefElementAddrInst_fieldIndex(BridgedInstruction reai) {
  return castToInst<RefElementAddrInst>(reai)->getFieldIndex();
}

SwiftInt PartialApplyInst_numArguments(BridgedInstruction pai) {
  return castToInst<PartialApplyInst>(pai)->getNumArguments();
}

SwiftInt ApplyInst_numArguments(BridgedInstruction ai) {
  return castToInst<ApplyInst>(ai)->getNumArguments();
}

SwiftInt BeginApplyInst_numArguments(BridgedInstruction tai) {
  return castToInst<BeginApplyInst>(tai)->getNumArguments();
}

SwiftInt TryApplyInst_numArguments(BridgedInstruction tai) {
  return castToInst<TryApplyInst>(tai)->getNumArguments();
}

BridgedBasicBlock BranchInst_getTargetBlock(BridgedInstruction bi) {
  return {nullptr};
}

SwiftInt SwitchEnumInst_getNumCases(BridgedInstruction se) {
  return castToInst<SwitchEnumInst>(se)->getNumCases();
}

SwiftInt SwitchEnumInst_getCaseIndex(BridgedInstruction se, SwiftInt idx) {
  return getCaseIndex(castToInst<SwitchEnumInst>(se)->getCase(idx).first);
}

SwiftInt StoreInst_getStoreOwnership(BridgedInstruction store) {
  return (SwiftInt)castToInst<StoreInst>(store)->getOwnershipQualifier();
}


//===----------------------------------------------------------------------===//
//                                SILBuilder
//===----------------------------------------------------------------------===//

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedInstruction insertionPoint,
          BridgedLocation loc, BridgedStringRef name,
          BridgedType operandType, BridgedType resultType,
          BridgedValueArray arguments) {
    SILBuilder builder(castToInst(insertionPoint), getSILDebugScope(loc));
    SmallVector<SILValue, 16> argValues;
  return {nullptr};
}

BridgedInstruction SILBuilder_createCondFail(BridgedInstruction insertionPoint,
          BridgedLocation loc, BridgedValue condition, BridgedStringRef messge) {
  SILBuilder builder(castToInst(insertionPoint), getSILDebugScope(loc));
  return {nullptr};
}

BridgedInstruction SILBuilder_createIntegerLiteral(BridgedInstruction insertionPoint,
          BridgedLocation loc, BridgedType type, SwiftInt value) {
  SILBuilder builder(castToInst(insertionPoint), getSILDebugScope(loc));
  return {nullptr};
}

