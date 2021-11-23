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

// Does return null, if libswift is not used, i.e. initializeLibSwift() is
// never called.
SwiftMetatype SILNode::getSILNodeMetatype(SILNodeKind kind) {
  SwiftMetatype metatype = nodeMetatypes[(unsigned)kind];
  assert((!nodeMetatypesInitialized || metatype) &&
        "no metatype for bridged SIL node");
  return metatype;
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
    storage.push_back(static_cast<ValueBase *>(base[idx * 2]));
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
//  nodeMetatypesInitialized = true;

  // Handle the important non Node classes.
  StringRef clName = getStringRef(className);
  if (clName == "Function")
    return SILFunction::registerBridgedMetatype(metatype);
  if (clName == "BasicBlock")
    return SILBasicBlock::registerBridgedMetatype(metatype);
  if (clName == "GlobalVariable")
    return SILGlobalVariable::registerBridgedMetatype(metatype);
  if (clName == "BlockArgument") {
    nodeMetatypes[(unsigned)SILNodeKind::SILPhiArgument] = metatype;
    return;
  }
  if (clName == "FunctionArgument") {
    nodeMetatypes[(unsigned)SILNodeKind::SILFunctionArgument] = metatype;
    return;
  }

  // Pre-populate the "unimplemented" ranges of metatypes.
  // If a specifc class is not implemented yet in libswift, it bridges to an
  // "unimplemented" class. This ensures that optimizations handle _all_ kind of
  // instructions gracefully, without the need to define the not-yet-used
  // classes in libswift.
#define VALUE_RANGE(ID) SILNodeKind::First_##ID, SILNodeKind::Last_##ID
  if (clName == "UnimplementedRefCountingInst")
    return setUnimplementedRange(metatype, VALUE_RANGE(RefCountingInst));
  if (clName == "UnimplementedSingleValueInst")
    return setUnimplementedRange(metatype, VALUE_RANGE(SingleValueInstruction));
  if (clName == "UnimplementedInstruction")
    return setUnimplementedRange(metatype, VALUE_RANGE(SILInstruction));
#undef VALUE_RANGE

  if (valueNamesToKind.empty()) {
#define VALUE(ID, PARENT) \
    valueNamesToKind[#ID] = SILNodeKind::ID;
#define BRIDGED_NON_VALUE_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE) \
    VALUE(ID, NAME)
#define ARGUMENT(ID, PARENT) \
    VALUE(ID, NAME)
#define BRIDGED_SINGLE_VALUE_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE) \
    VALUE(ID, NAME)
#define MULTIPLE_VALUE_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE) \
    VALUE(ID, NAME)
#include "swift/SIL/SILNodes.def"
  }

  std::string prefixedName;
  auto iter = valueNamesToKind.find(clName);
  if (iter == valueNamesToKind.end()) {
    // Try again with a "SIL" prefix. For example Argument -> SILArgument.
    prefixedName = std::string("SIL") + std::string(clName);
    iter = valueNamesToKind.find(prefixedName);
    if (iter == valueNamesToKind.end()) {
      llvm::errs() << "Unknown bridged node class " << clName << '\n';
      abort();
    }
    clName = prefixedName;
  }
  SILNodeKind kind = iter->second;
  SwiftMetatype existingTy = nodeMetatypes[(unsigned)kind];
  if (existingTy && !unimplementedTypes.count(existingTy)) {
    llvm::errs() << "Double registration of class " << clName << '\n';
    abort();
  }
  nodeMetatypes[(unsigned)kind] = metatype;
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

BridgedStringRef SILFunction_getName(SILFunction *function) {
  llvm::dbgs() << "Getting name: ";
  llvm::dbgs() << function->getName() << "\n";
  return getBridgedStringRef(function->getName());
}

std::string SILFunction_debugDescription(SILFunction *function) {
  std::string str;
  llvm::raw_string_ostream os(str);
  function->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

SILBasicBlock * _Nullable SILFunction_firstBlock(SILFunction *f) {
  if (f->empty())
    return nullptr;
//  f->getEntryBlock()->dump();
  return &*f->begin();
}

OptionalRef<SILBasicBlock> SILFunction_lastBlock(SILFunction *f) {
  if (f->empty())
    return nullptr;
  return &*f->rbegin();
}

SwiftInt SILFunction_numIndirectResultArguments(SILFunction *function) {
  return function->getLoweredFunctionType()->getNumIndirectFormalResults();
}

SwiftInt SILFunction_getSelfArgumentIndex(SILFunction *function) {
  CanSILFunctionType fTy = function->getLoweredFunctionType();
  if (!fTy->hasSelfParam())
    return -1;
  return fTy->getNumParameters() + fTy->getNumIndirectFormalResults() - 1;
}

//===----------------------------------------------------------------------===//
//                               SILBasicBlock
//===----------------------------------------------------------------------===//

static_assert(BridgedSuccessorSize == sizeof(SILSuccessor),
              "wrong bridged SILSuccessor size");

int SILBasicBlock_mytest(SILBasicBlock *b)  {
  llvm::dbgs() << "starting my test\n";
  auto iter = b->getIterator();
  while (iter != b->getParent()->end()) {
    iter->dump();
    iter = std::next(iter);
    llvm::dbgs() << "in my test\n";
  }
  return 42;
}

OptionalRef<SILBasicBlock> SILBasicBlock_next(SILBasicBlock *b) {
  auto iter = std::next(b->getIterator());
  if (iter == b->getParent()->end())
    return nullptr;
  return &*iter;
}

OptionalRef<SILBasicBlock> SILBasicBlock_previous(SILBasicBlock *b) {
  auto iter = std::next(b->getReverseIterator());
  if (iter == b->getParent()->rend())
    return nullptr;
  return &*iter;
}

SILFunction *SILBasicBlock_getFunction(SILBasicBlock *block) {
  return block->getParent();
}

std::string SILBasicBlock_debugDescription(SILBasicBlock *block) {
  std::string str;
  llvm::raw_string_ostream os(str);
  block->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

SILInstruction *SILBasicBlock_firstInst(SILBasicBlock *b) {
  if (b->empty())
    return nullptr;
  return &b->front();
}

OptionalRef<SILInstruction> SILBasicBlock_lastInst(SILBasicBlock *b) {
  if (b->empty())
    return nullptr;
  return &b->back();
}

SwiftInt SILBasicBlock_getNumArguments(SILBasicBlock *block) {
  return block->getNumArguments();
}

SILArgument *SILBasicBlock_getArgument(SILBasicBlock *block, SwiftInt index) {
  return block->getArgument(index);
}

OptionalBridgedSuccessor SILBasicBlock_getFirstPred(SILBasicBlock *block) {
  return {block->pred_begin().getSuccessorRef()};
}

static SILSuccessor *castToSuccessor(BridgedSuccessor succ) {
  return const_cast<SILSuccessor *>(static_cast<const SILSuccessor *>(succ.succ));
}

OptionalBridgedSuccessor SILSuccessor_getNext(BridgedSuccessor succ) {
  return {castToSuccessor(succ)->getNext()};
}

SILBasicBlock *SILSuccessor_getTargetBlock(BridgedSuccessor succ) {
  return castToSuccessor(succ)->getBB();
}

SILInstruction *SILSuccessor_getContainingInst(BridgedSuccessor succ) {
  return castToSuccessor(succ)->getContainingInst();
}

//===----------------------------------------------------------------------===//
//                                SILArgument
//===----------------------------------------------------------------------===//

SILBasicBlock *SILArgument_getParent(SILArgument *argument) {
  return argument->getParent();
}

//===----------------------------------------------------------------------===//
//                                SILValue
//===----------------------------------------------------------------------===//

static_assert(BridgedOperandSize == sizeof(Operand),
              "wrong bridged Operand size");

std::string SILNode_debugDescription(ValueBase *node) {
  std::string str;
  llvm::raw_string_ostream os(str);
  node->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

std::string SILInstruction_debugDescription(SILInstruction *i) {
  i->dump();
  std::string str;
  llvm::raw_string_ostream os(str);
  i->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

static Operand *castToOperand(BridgedOperand operand) {
  return const_cast<Operand *>(static_cast<const Operand *>(operand.op));
}

ValueBase *Operand_getValue(BridgedOperand operand) {
  return {castToOperand(operand)->get()};
}

OptionalBridgedOperand Operand_nextUse(BridgedOperand operand) {
  return {castToOperand(operand)->getNextUse()};
}

SILInstruction *Operand_getUser(BridgedOperand operand) {
  return castToOperand(operand)->getUser()->asSILNode()->castToInstruction();
}

SwiftInt Operand_isTypeDependent(BridgedOperand operand) {
  return castToOperand(operand)->isTypeDependent() ? 1 : 0;
}

OptionalBridgedOperand SILValue_firstUse(ValueBase *value) {
  return {*value->use_begin()};
}

SILType SILValue_getType(ValueBase *value) {
  return value->getType();
}

//===----------------------------------------------------------------------===//
//                            SILType
//===----------------------------------------------------------------------===//

SwiftInt SILType_isAddress(SILType type) {
  return type.isAddress();
}

SwiftInt SILType_isTrivial(SILType type, SILFunction *function) {
  return type.isTrivial(*function);
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

OptionalRef<SILInstruction> SILInstruction_next(SILInstruction *i) {
  auto iter = std::next(i->getIterator());
  if (iter == i->getParent()->end())
    return nullptr;
  return cast<SILInstruction>(iter->asSILNode());
}

OptionalRef<SILInstruction> SILInstruction_previous(SILInstruction *i) {
  auto iter = std::next(i->getReverseIterator());
  if (iter == i->getParent()->rend())
    return nullptr;
  return cast<SILInstruction>(iter->asSILNode());
}

SILBasicBlock *SILInstruction_getParent(SILInstruction *i) {
  assert(!i->isStaticInitializerInst() &&
         "cannot get the parent of a static initializer instruction");
  return i->getParent();
}

BridgedArrayRef SILInstruction_getOperands(SILInstruction *inst) {
  auto operands = inst->getAllOperands();
  return {(const unsigned char *)operands.data(), operands.size()};
}

void SILInstruction_setOperand(SILInstruction *inst, SwiftInt index,
                               ValueBase *value) {
  inst->setOperand((unsigned)index, value);
}

BridgedLocation SILInstruction_getLocation(SILInstruction *inst) {
  SILDebugLocation loc = inst->getDebugLocation();
  return *reinterpret_cast<BridgedLocation *>(&loc);
}

BridgedMemoryBehavior SILInstruction_getMemBehavior(SILInstruction *inst) {
  return (BridgedMemoryBehavior)inst->getMemoryBehavior();
}

SILInstruction *MultiValueInstResult_getParent(BridgedMultiValueResult result) {
  return {static_cast<MultipleValueInstructionResult *>(result.obj)->getParent()};
}

SwiftInt MultipleValueInstruction_getNumResults(MultipleValueInstruction *inst) {
  return inst->getNumResults();
}
BridgedMultiValueResult
MultipleValueInstruction_getResult(MultipleValueInstruction *inst, SwiftInt index) {
  return {inst->getResult(index)};
}

BridgedArrayRef TermInst_getSuccessors(TermInst *term) {
  auto successors = term->getSuccessors();
  return {(const unsigned char *)successors.data(), successors.size()};
}

//===----------------------------------------------------------------------===//
//                            Instruction classes
//===----------------------------------------------------------------------===//

BridgedStringRef CondFailInst_getMessage(CondFailInst *cfi) {
  return getBridgedStringRef(cfi->getMessage());
}

BridgedGlobalVar GlobalAccessInst_getGlobal(GlobalAccessInst *globalInst) {
  return {globalInst->getReferencedGlobal()};
}

SwiftInt TupleExtractInst_fieldIndex(TupleExtractInst *tei) {
  return tei->getFieldIndex();
}

SwiftInt TupleElementAddrInst_fieldIndex(TupleElementAddrInst *teai) {
  return teai->getFieldIndex();
}

SwiftInt StructExtractInst_fieldIndex(StructExtractInst *sei) {
  return sei->getFieldIndex();
}

SwiftInt StructElementAddrInst_fieldIndex(StructElementAddrInst *seai) {
  return seai->getFieldIndex();
}

SwiftInt EnumInst_caseIndex(EnumInst *ei) {
  return getCaseIndex(ei->getElement());
}

SwiftInt UncheckedEnumDataInst_caseIndex(UncheckedEnumDataInst *uedi) {
  return getCaseIndex(uedi->getElement());
}

SwiftInt RefElementAddrInst_fieldIndex(RefElementAddrInst *reai) {
  return reai->getFieldIndex();
}

SwiftInt PartialApplyInst_numArguments(PartialApplyInst *pai) {
  return pai->getNumArguments();
}

SwiftInt ApplyInst_numArguments(ApplyInst *ai) {
  return ai->getNumArguments();
}

SwiftInt BeginApplyInst_numArguments(BeginApplyInst *tai) {
  return tai->getNumArguments();
}

SwiftInt TryApplyInst_numArguments(TryApplyInst *tai) {
  return tai->getNumArguments();
}

SILBasicBlock *BranchInst_getTargetBlock(BranchInst *bi) {
  return bi->getDestBB();
}

SwiftInt SwitchEnumInst_getNumCases(SwitchEnumInst *se) {
  return se->getNumCases();
}

SwiftInt SwitchEnumInst_getCaseIndex(SwitchEnumInst *se, SwiftInt idx) {
  return getCaseIndex(se->getCase(idx).first);
}

SwiftInt StoreInst_getStoreOwnership(StoreInst *store) {
  return (SwiftInt)store->getOwnershipQualifier();
}


//===----------------------------------------------------------------------===//
//                                SILBuilder
//===----------------------------------------------------------------------===//

SILInstruction *SILBuilder_createBuiltinBinaryFunction(
          SILInstruction *insertionPoint,
          BridgedLocation loc, BridgedStringRef name,
          SILType operandType, SILType resultType,
          BridgedValueArray arguments) {
    SILBuilder builder(insertionPoint, getSILDebugScope(loc));
    SmallVector<SILValue, 16> argValues;
    return {builder.createBuiltinBinaryFunction(getRegularLocation(loc),
      getStringRef(name), operandType, resultType,
      getSILValues(arguments, argValues))};
}

SILInstruction *SILBuilder_createCondFail(
          SILInstruction *insertionPoint, BridgedLocation loc,
          ValueBase *condition, BridgedStringRef messge) {
  SILBuilder builder(insertionPoint, getSILDebugScope(loc));
  return {builder.createCondFail(getRegularLocation(loc), condition,
          getStringRef(messge))};
}
