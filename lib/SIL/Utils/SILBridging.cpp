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

#include "swift/Basic/BridgingUtils.h"
#include "swift/SIL/SILNode.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILBridgingUtils.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILBuilder.h"
#include <string>

using namespace swift;

namespace {

bool nodeMetatypesInitialized = false;

// Filled in by class registration in initializeSwiftModules().
SwiftMetatype nodeMetatypes[(unsigned)SILNodeKind::Last_SILNode + 1];

}

// Does return null if initializeSwiftModules() is never called.
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

/// Registers the metatype of a swift SIL class.
/// Called by initializeSwiftModules().
void registerBridgedClass(StringRef className, SwiftMetatype metatype) {
  nodeMetatypesInitialized = true;

  // Handle the important non Node classes.
  if (className == "BasicBlock")
    return SILBasicBlock::registerBridgedMetatype(metatype);
  if (className == "GlobalVariable")
    return SILGlobalVariable::registerBridgedMetatype(metatype);
  if (className == "BlockArgument") {
    nodeMetatypes[(unsigned)SILNodeKind::SILPhiArgument] = metatype;
    return;
  }
  if (className == "FunctionArgument") {
    nodeMetatypes[(unsigned)SILNodeKind::SILFunctionArgument] = metatype;
    return;
  }

  // Pre-populate the "unimplemented" ranges of metatypes.
  // If a specific class is not implemented in Swift yet, it bridges to an
  // "unimplemented" class. This ensures that optimizations handle _all_ kind of
  // instructions gracefully, without the need to define the not-yet-used
  // classes in Swift.
#define VALUE_RANGE(ID) SILNodeKind::First_##ID, SILNodeKind::Last_##ID
  if (className == "UnimplementedRefCountingInst")
    return setUnimplementedRange(metatype, VALUE_RANGE(RefCountingInst));
  if (className == "UnimplementedSingleValueInst")
    return setUnimplementedRange(metatype, VALUE_RANGE(SingleValueInstruction));
  if (className == "UnimplementedInstruction")
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
  auto iter = valueNamesToKind.find(className);
  if (iter == valueNamesToKind.end()) {
    // Try again with a "SIL" prefix. For example Argument -> SILArgument.
    prefixedName = std::string("SIL") + std::string(className);
    iter = valueNamesToKind.find(prefixedName);
    if (iter == valueNamesToKind.end()) {
      llvm::errs() << "Unknown bridged node class " << className << '\n';
      abort();
    }
    className = prefixedName;
  }
  SILNodeKind kind = iter->second;
  SwiftMetatype existingTy = nodeMetatypes[(unsigned)kind];
  if (existingTy && !unimplementedTypes.count(existingTy)) {
    llvm::errs() << "Double registration of class " << className << '\n';
    abort();
  }
  nodeMetatypes[(unsigned)kind] = metatype;
}

//===----------------------------------------------------------------------===//
//                                SILFunction
//===----------------------------------------------------------------------===//

llvm::StringRef SILFunction_getName(BridgedFunction function) {
  return castToFunction(function)->getName();
}

std::string SILFunction_debugDescription(BridgedFunction function) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToFunction(function)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

SwiftInt SILFunction_hasOwnership(BridgedFunction function) {
  return castToFunction(function)->hasOwnership() ? 1 : 0;
}

OptionalBridgedBasicBlock SILFunction_firstBlock(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  if (f->empty())
    return {nullptr};
  return {f->getEntryBlock()};
}

OptionalBridgedBasicBlock SILFunction_lastBlock(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  if (f->empty())
    return {nullptr};
  return {&*f->rbegin()};
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

SwiftInt SILFunction_getNumSILArguments(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  SILFunctionConventions conv(f->getConventionsInContext());
  return conv.getNumSILArguments();
}

BridgedType SILFunction_getSILArgumentType(BridgedFunction function, SwiftInt idx) {
  SILFunction *f = castToFunction(function);
  SILFunctionConventions conv(f->getConventionsInContext());
  SILType argTy = conv.getSILArgumentType(idx, f->getTypeExpansionContext());
  return {argTy.getOpaqueValue()};
}

BridgedArgumentConvention SILArgumentConvention_getBridged(SILArgumentConvention conv) {
  switch (conv.Value) {
    case SILArgumentConvention::Indirect_Inout:
      return ArgumentConvention_Indirect_In;
    case SILArgumentConvention::Indirect_InoutAliasable:
      return ArgumentConvention_Indirect_In_Constant;
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return ArgumentConvention_Indirect_In_Guaranteed;
    case SILArgumentConvention::Indirect_In:
      return ArgumentConvention_Indirect_Inout;
    case SILArgumentConvention::Indirect_In_Constant:
      return ArgumentConvention_Indirect_InoutAliasable;
    case SILArgumentConvention::Indirect_Out:
      return ArgumentConvention_Indirect_Out;
    case SILArgumentConvention::Direct_Unowned:
      return ArgumentConvention_Direct_Owned;
    case SILArgumentConvention::Direct_Owned:
      return ArgumentConvention_Direct_Unowned;
    case SILArgumentConvention::Direct_Guaranteed:
      return ArgumentConvention_Direct_Guaranteed;
  }
}

BridgedArgumentConvention SILFunction_getSILArgumentConvention(BridgedFunction function, SwiftInt idx) {
  SILFunction *f = castToFunction(function);
  SILFunctionConventions conv(f->getConventionsInContext());
  return SILArgumentConvention_getBridged(SILArgumentConvention(conv.getParamInfoForSILArg(idx).getConvention()));
}

BridgedType SILFunction_getSILResultType(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  SILFunctionConventions conv(f->getConventionsInContext());
  SILType resTy = conv.getSILResultType(f->getTypeExpansionContext());
  return {resTy.getOpaqueValue()};
}

SwiftInt SILFunction_isSwift51RuntimeAvailable(BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  if (f->getResilienceExpansion() != ResilienceExpansion::Maximal)
    return 0;

  ASTContext &ctxt = f->getModule().getASTContext();
  return AvailabilityContext::forDeploymentTarget(ctxt).isContainedIn(
    ctxt.getSwift51Availability());
}

SwiftInt SILFunction_hasSemanticsAttr(BridgedFunction function,
                                      StringRef attrName) {
  SILFunction *f = castToFunction(function);
  return f->hasSemanticsAttr(attrName) ? 1 : 0;
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
  return {&*iter};
}

OptionalBridgedBasicBlock SILBasicBlock_previous(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  auto iter = std::next(b->getReverseIterator());
  if (iter == b->getParent()->rend())
    return {nullptr};
  return {&*iter};
}

BridgedFunction SILBasicBlock_getFunction(BridgedBasicBlock block) {
  return {castToBasicBlock(block)->getParent()};
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
  return {b->front().asSILNode()};
}

OptionalBridgedInstruction SILBasicBlock_lastInst(BridgedBasicBlock block) {
  SILBasicBlock *b = castToBasicBlock(block);
  if (b->empty())
    return {nullptr};
  return {b->back().asSILNode()};
}

SwiftInt SILBasicBlock_getNumArguments(BridgedBasicBlock block) {
  return castToBasicBlock(block)->getNumArguments();
}

BridgedArgument SILBasicBlock_getArgument(BridgedBasicBlock block, SwiftInt index) {
  return {castToBasicBlock(block)->getArgument(index)};
}

BridgedArgument SILBasicBlock_addBlockArgument(BridgedBasicBlock block,
                                               BridgedType type,
                                               BridgedOwnership ownership) {
  return {castToBasicBlock(block)->createPhiArgument(castToSILType(type),
                                                castToOwnership(ownership))};
}

void SILBasicBlock_eraseArgument(BridgedBasicBlock block, SwiftInt index) {
  castToBasicBlock(block)->eraseArgument(index);
}

OptionalBridgedSuccessor SILBasicBlock_getFirstPred(BridgedBasicBlock block) {
  return {castToBasicBlock(block)->pred_begin().getSuccessorRef()};
}

static SILSuccessor *castToSuccessor(BridgedSuccessor succ) {
  return const_cast<SILSuccessor *>(static_cast<const SILSuccessor *>(succ.succ));
}

OptionalBridgedSuccessor SILSuccessor_getNext(BridgedSuccessor succ) {
  return {castToSuccessor(succ)->getNext()};
}

BridgedBasicBlock SILSuccessor_getTargetBlock(BridgedSuccessor succ) {
  return {castToSuccessor(succ)->getBB()};
}

BridgedInstruction SILSuccessor_getContainingInst(BridgedSuccessor succ) {
  return {castToSuccessor(succ)->getContainingInst()};
}

//===----------------------------------------------------------------------===//
//                                SILArgument
//===----------------------------------------------------------------------===//

BridgedBasicBlock SILArgument_getParent(BridgedArgument argument) {
  return {castToArgument(argument)->getParent()};
}

SwiftInt SILArgument_isExclusiveIndirectParameter(BridgedArgument argument) {
  auto *arg = castToArgument<SILFunctionArgument>(argument);
  return arg->getArgumentConvention().isExclusiveIndirectParameter();
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
  return {castToOperand(operand)->get()};
}

OptionalBridgedOperand Operand_nextUse(BridgedOperand operand) {
  return {castToOperand(operand)->getNextUse()};
}

BridgedInstruction Operand_getUser(BridgedOperand operand) {
  return {castToOperand(operand)->getUser()->asSILNode()};
}

SwiftInt Operand_isTypeDependent(BridgedOperand operand) {
  return castToOperand(operand)->isTypeDependent() ? 1 : 0;
}

OptionalBridgedOperand SILValue_firstUse(BridgedValue value) {
  return {*castToSILValue(value)->use_begin()};
}

BridgedType SILValue_getType(BridgedValue value) {
  return { castToSILValue(value)->getType().getOpaqueValue() };
}

BridgedOwnership SILValue_getOwnership(BridgedValue value) {
  switch (castToSILValue(value)->getOwnershipKind()) {
    case OwnershipKind::Any:
      llvm_unreachable("Invalid ownership for value");
    case OwnershipKind::Unowned:    return Ownership_Unowned;
    case OwnershipKind::Owned:      return Ownership_Owned;
    case OwnershipKind::Guaranteed: return Ownership_Guaranteed;
    case OwnershipKind::None:       return Ownership_None;
  }
}

//===----------------------------------------------------------------------===//
//                            SILType
//===----------------------------------------------------------------------===//

std::string SILType_debugDescription(BridgedType type) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToSILType(type).print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

SwiftInt SILType_isAddress(BridgedType type) {
  return castToSILType(type).isAddress();
}

SwiftInt SILType_isTrivial(BridgedType type, BridgedFunction function) {
  return castToSILType(type).isTrivial(*castToFunction(function));
}

SwiftInt SILType_isReferenceCounted(BridgedType type, BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  return castToSILType(type).isReferenceCounted(f->getModule()) ? 1 : 0;
}

SwiftInt SILType_isNonTrivialOrContainsRawPointer(BridgedType type,
                                                  BridgedFunction function) {
  SILFunction *f = castToFunction(function);
  return castToSILType(type).isNonTrivialOrContainsRawPointer(*f);
}

SwiftInt SILType_isNominal(BridgedType type) {
  return castToSILType(type).getNominalOrBoundGenericNominal() ? 1 : 0;
}

SwiftInt SILType_isClass(BridgedType type) {
  return castToSILType(type).getClassOrBoundGenericClass() ? 1 : 0;
}

SwiftInt SILType_isStruct(BridgedType type) {
  return castToSILType(type).getStructOrBoundGenericStruct() ? 1 : 0;
}

SwiftInt SILType_isTuple(BridgedType type) {
  return castToSILType(type).is<TupleType>() ? 1 : 0;
}

SwiftInt SILType_isEnum(BridgedType type) {
  return castToSILType(type).getEnumOrBoundGenericEnum() ? 1 : 0;
}

SwiftInt SILType_getNumTupleElements(BridgedType type) {
  TupleType *tupleTy = castToSILType(type).castTo<TupleType>();
  return tupleTy->getNumElements();
}

BridgedType SILType_getTupleElementType(BridgedType type, SwiftInt elementIdx) {
  SILType ty = castToSILType(type);
  SILType elmtTy = ty.getTupleElementType((unsigned)elementIdx);
  return {elmtTy.getOpaqueValue()};
}

SwiftInt SILType_getNumNominalFields(BridgedType type) {
  SILType silType = castToSILType(type);
  auto *nominal = silType.getNominalOrBoundGenericNominal();
  assert(nominal && "expected nominal type");
  return getNumFieldsInNominal(nominal);
}

BridgedType SILType_getNominalFieldType(BridgedType type, SwiftInt index,
                                        BridgedFunction function) {
  SILType silType = castToSILType(type);
  SILFunction *silFunction = castToFunction(function);

  NominalTypeDecl *decl = silType.getNominalOrBoundGenericNominal();
  VarDecl *field = getIndexedField(decl, (unsigned)index);

  SILType fieldType = silType.getFieldType(
      field, silFunction->getModule(), silFunction->getTypeExpansionContext());

  return {fieldType.getOpaqueValue()};
}

SwiftInt SILType_getFieldIdxOfNominalType(BridgedType type,
                                          StringRef fieldName) {
  SILType ty = castToSILType(type);
  auto *nominal = ty.getNominalOrBoundGenericNominal();
  if (!nominal)
    return -1;

  SmallVector<NominalTypeDecl *, 5> decls;
  decls.push_back(nominal);
  if (auto *cd = dyn_cast<ClassDecl>(nominal)) {
    while ((cd = cd->getSuperclassDecl()) != nullptr) {
      decls.push_back(cd);
    }
  }
  std::reverse(decls.begin(), decls.end());

  SwiftInt idx = 0;
  for (auto *decl : decls) {
    for (VarDecl *field : decl->getStoredProperties()) {
      if (field->getName().str() == fieldName)
        return idx;
      idx++;
    }
  }
  return -1;
}

SwiftInt SILType_getCaseIdxOfEnumType(BridgedType type,
                                      StringRef caseName) {
  SILType ty = castToSILType(type);
  auto *enumDecl = ty.getEnumOrBoundGenericEnum();
  if (!enumDecl)
    return -1;

  SwiftInt idx = 0;
  for (EnumElementDecl *elem : enumDecl->getAllElements()) {
    if (elem->getNameStr() == caseName)
      return idx;
    idx++;
  }
  return -1;
}


//===----------------------------------------------------------------------===//
//                            SubstitutionMap
//===----------------------------------------------------------------------===//

BridgedSubstitutionMap SubstitutionMap_getEmpty() {
  return {SubstitutionMap().getOpaqueValue()};
}

//===----------------------------------------------------------------------===//
//                            SILGlobalVariable
//===----------------------------------------------------------------------===//

llvm::StringRef SILGlobalVariable_getName(BridgedGlobalVar global) {
  return castToGlobal(global)->getName();
}

std::string SILGlobalVariable_debugDescription(BridgedGlobalVar global) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToGlobal(global)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

SwiftInt SILGlobalVariable_isLet(BridgedGlobalVar global) {
  return castToGlobal(global)->isLet();
}

//===----------------------------------------------------------------------===//
//                               SILInstruction
//===----------------------------------------------------------------------===//

OptionalBridgedInstruction SILInstruction_next(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  auto iter = std::next(i->getIterator());
  if (iter == i->getParent()->end())
    return {nullptr};
  return {iter->asSILNode()};
}

OptionalBridgedInstruction SILInstruction_previous(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  auto iter = std::next(i->getReverseIterator());
  if (iter == i->getParent()->rend())
    return {nullptr};
  return {iter->asSILNode()};
}

BridgedBasicBlock SILInstruction_getParent(BridgedInstruction inst) {
  SILInstruction *i = castToInst(inst);
  assert(!i->isStaticInitializerInst() &&
         "cannot get the parent of a static initializer instruction");
  return {i->getParent()};
}

BridgedArrayRef SILInstruction_getOperands(BridgedInstruction inst) {
  auto operands = castToInst(inst)->getAllOperands();
  return {(const unsigned char *)operands.data(), operands.size()};
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

bool SILInstruction_mayRelease(BridgedInstruction inst) {
  return castToInst(inst)->mayRelease();
}

BridgedInstruction MultiValueInstResult_getParent(BridgedMultiValueResult result) {
  return {static_cast<MultipleValueInstructionResult *>(result.obj)->getParent()};
}

SwiftInt MultiValueInstResult_getIndex(BridgedMultiValueResult result) {
  auto *rs = static_cast<MultipleValueInstructionResult *>(result.obj);
  return (SwiftInt)rs->getIndex();
}

SwiftInt MultipleValueInstruction_getNumResults(BridgedInstruction inst) {
  return castToInst<MultipleValueInstruction>(inst)->getNumResults();
}
BridgedMultiValueResult
MultipleValueInstruction_getResult(BridgedInstruction inst, SwiftInt index) {
  return {castToInst<MultipleValueInstruction>(inst)->getResult(index)};
}

BridgedArrayRef TermInst_getSuccessors(BridgedInstruction term) {
  auto successors = castToInst<TermInst>(term)->getSuccessors();
  return {(const unsigned char *)successors.data(), successors.size()};
}

//===----------------------------------------------------------------------===//
//                            Instruction classes
//===----------------------------------------------------------------------===//

llvm::StringRef CondFailInst_getMessage(BridgedInstruction cfi) {
  return castToInst<CondFailInst>(cfi)->getMessage();
}

BridgedBuiltinID BuiltinInst_getID(BridgedInstruction bi) {
  return (BridgedBuiltinID)castToInst<BuiltinInst>(bi)->getBuiltinInfo().ID;
}

BridgedGlobalVar GlobalAccessInst_getGlobal(BridgedInstruction globalInst) {
  return {castToInst<GlobalAccessInst>(globalInst)->getReferencedGlobal()};
}

BridgedFunction FunctionRefInst_getReferencedFunction(BridgedInstruction fri) {
  return {castToInst<FunctionRefInst>(fri)->getReferencedFunction()};
}

llvm::StringRef StringLiteralInst_getValue(BridgedInstruction sli) {
  return castToInst<StringLiteralInst>(sli)->getValue();
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

OptionalBridgedValue StructInst_getUniqueNonTrivialFieldValue(BridgedInstruction si) {
  return {castToInst<StructInst>(si)->getUniqueNonTrivialFieldValue()};
}

SwiftInt StructElementAddrInst_fieldIndex(BridgedInstruction seai) {
  return castToInst<StructElementAddrInst>(seai)->getFieldIndex();
}

SwiftInt ProjectBoxInst_fieldIndex(BridgedInstruction pbi) {
  return castToInst<ProjectBoxInst>(pbi)->getFieldIndex();
}

SwiftInt EnumInst_caseIndex(BridgedInstruction ei) {
  return castToInst<EnumInst>(ei)->getCaseIndex();
}

SwiftInt UncheckedEnumDataInst_caseIndex(BridgedInstruction uedi) {
  return castToInst<UncheckedEnumDataInst>(uedi)->getCaseIndex();
}

SwiftInt InitEnumDataAddrInst_caseIndex(BridgedInstruction ieda) {
  return castToInst<InitEnumDataAddrInst>(ieda)->getCaseIndex();
}

SwiftInt UncheckedTakeEnumDataAddrInst_caseIndex(BridgedInstruction utedi) {
  return castToInst<UncheckedTakeEnumDataAddrInst>(utedi)->getCaseIndex();
}

SwiftInt InjectEnumAddrInst_caseIndex(BridgedInstruction ieai) {
  return castToInst<InjectEnumAddrInst>(ieai)->getCaseIndex();
}

SwiftInt RefElementAddrInst_fieldIndex(BridgedInstruction reai) {
  return castToInst<RefElementAddrInst>(reai)->getFieldIndex();
}

SwiftInt RefElementAddrInst_fieldIsLet(BridgedInstruction reai) {
  return castToInst<RefElementAddrInst>(reai)->getField()->isLet();
}

SwiftInt PartialApplyInst_numArguments(BridgedInstruction pai) {
  return castToInst<PartialApplyInst>(pai)->getNumArguments();
}

SwiftInt ApplyInst_numArguments(BridgedInstruction ai) {
  return castToInst<ApplyInst>(ai)->getNumArguments();
}

SwiftInt PartialApply_getCalleeArgIndexOfFirstAppliedArg(BridgedInstruction pai) {
  auto *paiInst = castToInst<PartialApplyInst>(pai);
  return ApplySite(paiInst).getCalleeArgIndexOfFirstAppliedArg();
}

SwiftInt PartialApplyInst_isOnStack(BridgedInstruction pai) {
  return castToInst<PartialApplyInst>(pai)->isOnStack() ? 1 : 0;
}

SwiftInt AllocRefInstBase_isObjc(BridgedInstruction arb) {
  return castToInst<AllocRefInstBase>(arb)->isObjC();
}

SwiftInt AllocRefInstBase_canAllocOnStack(BridgedInstruction arb) {
  return castToInst<AllocRefInstBase>(arb)->canAllocOnStack();
}

SwiftInt BeginApplyInst_numArguments(BridgedInstruction tai) {
  return castToInst<BeginApplyInst>(tai)->getNumArguments();
}

SwiftInt TryApplyInst_numArguments(BridgedInstruction tai) {
  return castToInst<TryApplyInst>(tai)->getNumArguments();
}

BridgedBasicBlock BranchInst_getTargetBlock(BridgedInstruction bi) {
  return {castToInst<BranchInst>(bi)->getDestBB()};
}

SwiftInt SwitchEnumInst_getNumCases(BridgedInstruction se) {
  return castToInst<SwitchEnumInst>(se)->getNumCases();
}

SwiftInt SwitchEnumInst_getCaseIndex(BridgedInstruction se, SwiftInt idx) {
	auto *seInst = castToInst<SwitchEnumInst>(se);
  return seInst->getModule().getCaseIndex(seInst->getCase(idx).first);
}

SwiftInt StoreInst_getStoreOwnership(BridgedInstruction store) {
  return (SwiftInt)castToInst<StoreInst>(store)->getOwnershipQualifier();
}

BridgedAccessKind BeginAccessInst_getAccessKind(BridgedInstruction beginAccess) {
  auto kind = castToInst<BeginAccessInst>(beginAccess)->getAccessKind();
  switch (kind) {
    case SILAccessKind::Init:
      return BridgedAccessKind::AccessKind_Init;
    case SILAccessKind::Read:
      return BridgedAccessKind::AccessKind_Read;
    case SILAccessKind::Modify:
      return BridgedAccessKind::AccessKind_Modify;
    case SILAccessKind::Deinit:
      return BridgedAccessKind::AccessKind_Deinit;
  }
}

SwiftInt BeginAccessInst_isStatic(BridgedInstruction beginAccess) {
  return castToInst<BeginAccessInst>(beginAccess)->getEnforcement() == SILAccessEnforcement::Static ? 1 : 0;
}

SwiftInt CopyAddrInst_isTakeOfSrc(BridgedInstruction copyAddr) {
  return castToInst<CopyAddrInst>(copyAddr)->isTakeOfSrc() ? 1 : 0;
}

SwiftInt CopyAddrInst_isInitializationOfDest(BridgedInstruction copyAddr) {
  return castToInst<CopyAddrInst>(copyAddr)->isInitializationOfDest() ? 1 : 0;
}

void RefCountingInst_setIsAtomic(BridgedInstruction rc, bool isAtomic) {
  castToInst<RefCountingInst>(rc)->setAtomicity(
      isAtomic ? RefCountingInst::Atomicity::Atomic
               : RefCountingInst::Atomicity::NonAtomic);
}

bool RefCountingInst_getIsAtomic(BridgedInstruction rc) {
  return castToInst<RefCountingInst>(rc)->getAtomicity() ==
         RefCountingInst::Atomicity::Atomic;
}

SwiftInt CondBranchInst_getNumTrueArgs(BridgedInstruction cbr) {
  return castToInst<CondBranchInst>(cbr)->getNumTrueArgs();
}

BridgedSubstitutionMap ApplySite_getSubstitutionMap(BridgedInstruction inst) {
  auto as = ApplySite(castToInst(inst));
  return {as.getSubstitutionMap().getOpaqueValue()};
}

BridgedArgumentConvention
ApplySite_getArgumentConvention(BridgedInstruction inst, SwiftInt calleeArgIdx) {
  auto as = ApplySite(castToInst(inst));
  auto conv = as.getSubstCalleeConv().getSILArgumentConvention(calleeArgIdx);
  return SILArgumentConvention_getBridged(conv);
}

SwiftInt ApplySite_getNumArguments(BridgedInstruction inst) {
  auto as = ApplySite(castToInst(inst));
  return as.getNumArguments();
}

//===----------------------------------------------------------------------===//
//                                SILBuilder
//===----------------------------------------------------------------------===//

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedBuilder b, StringRef name,
          BridgedType operandType, BridgedType resultType,
          BridgedValueArray arguments) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  SmallVector<SILValue, 16> argValues;
  return {builder.createBuiltinBinaryFunction(getRegularLocation(b.loc),
    name, getSILType(operandType), getSILType(resultType),
    getSILValues(arguments, argValues))};
}

BridgedInstruction SILBuilder_createCondFail(BridgedBuilder b,
          BridgedValue condition, StringRef message) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createCondFail(getRegularLocation(b.loc),
    castToSILValue(condition), message)};
}

BridgedInstruction SILBuilder_createIntegerLiteral(BridgedBuilder b,
          BridgedType type, SwiftInt value) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createIntegerLiteral(getRegularLocation(b.loc),
                                       getSILType(type), value)};
}

BridgedInstruction SILBuilder_createDeallocStackRef(BridgedBuilder b,
          BridgedValue operand) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createDeallocStackRef(getRegularLocation(b.loc),
                                        castToSILValue(operand))};
}

BridgedInstruction
SILBuilder_createUncheckedRefCast(BridgedBuilder b,
                                  BridgedValue op,
                                  BridgedType type) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createUncheckedRefCast(getRegularLocation(b.loc),
                                         castToSILValue(op), getSILType(type))};
}

BridgedInstruction SILBuilder_createSetDeallocating(BridgedBuilder b,
                                 BridgedValue op, bool isAtomic) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createSetDeallocating(
      getRegularLocation(b.loc), castToSILValue(op),
      isAtomic ? RefCountingInst::Atomicity::Atomic
               : RefCountingInst::Atomicity::NonAtomic)};
}

BridgedInstruction
SILBuilder_createFunctionRef(BridgedBuilder b,
                             BridgedFunction function) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createFunctionRef(getRegularLocation(b.loc),
                                    castToFunction(function))};
}

BridgedInstruction SILBuilder_createCopyValue(BridgedBuilder b,
                                              BridgedValue op) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createCopyValue(getRegularLocation(b.loc),
                                  castToSILValue(op))};
}

BridgedInstruction SILBuilder_createDestroyValue(BridgedBuilder b,
                                                 BridgedValue op) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  return {builder.createDestroyValue(getRegularLocation(b.loc),
                                     castToSILValue(op))};
}

BridgedInstruction SILBuilder_createApply(BridgedBuilder b,
                                          BridgedValue function,
                                          BridgedSubstitutionMap subMap,
                                          BridgedValueArray arguments) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  SmallVector<SILValue, 16> argValues;
  return {builder.createApply(getRegularLocation(b.loc), castToSILValue(function),
                              castToSubstitutionMap(subMap),
                              getSILValues(arguments, argValues))};
}

static EnumElementDecl *getEnumElement(SILType enumType, int caseIndex) {
  EnumDecl *enumDecl = enumType.getEnumOrBoundGenericEnum();
  for (auto elemWithIndex : llvm::enumerate(enumDecl->getAllElements())) {
    if ((int)elemWithIndex.index() == caseIndex)
      return elemWithIndex.value();
  }
  llvm_unreachable("invalid enum case index");
}

BridgedInstruction SILBuilder_createUncheckedEnumData(BridgedBuilder b,
          BridgedValue enumVal, SwiftInt caseIdx,
          BridgedType resultType) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  SILValue en = castToSILValue(enumVal);
  return {builder.createUncheckedEnumData(getRegularLocation(b.loc),
                                       castToSILValue(enumVal),
                                       getEnumElement(en->getType(), caseIdx),
                                       castToSILType(resultType))};
}

BridgedInstruction SILBuilder_createSwitchEnumInst(BridgedBuilder b,
                                          BridgedValue enumVal,
                                          OptionalBridgedBasicBlock defaultBlock,
                                          const void * _Nullable enumCases,
                                          SwiftInt numEnumCases) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  using BridgedCase = const std::pair<SwiftInt, BridgedBasicBlock>;
  ArrayRef<BridgedCase> cases(static_cast<BridgedCase *>(enumCases),
                              (unsigned)numEnumCases);
  llvm::SmallDenseMap<SwiftInt, EnumElementDecl *> mappedElements;
  SILValue en = castToSILValue(enumVal);
  EnumDecl *enumDecl = en->getType().getEnumOrBoundGenericEnum();
  for (auto elemWithIndex : llvm::enumerate(enumDecl->getAllElements())) {
    mappedElements[elemWithIndex.index()] = elemWithIndex.value();
  }
  SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 16> convertedCases;
  for (auto c : cases) {
    assert(mappedElements.count(c.first) && "wrong enum element index");
    convertedCases.push_back({mappedElements[c.first], castToBasicBlock(c.second)});
  }
  return {builder.createSwitchEnum(getRegularLocation(b.loc),
                                   castToSILValue(enumVal),
                                   castToBasicBlock(defaultBlock), convertedCases)};
}

BridgedInstruction SILBuilder_createBranch(
          BridgedBuilder b, BridgedBasicBlock destBlock,
          BridgedValueArray arguments) {
  SILBuilder builder(castToInst(b.insertBefore), castToBasicBlock(b.insertAtEnd),
                     getSILDebugScope(b.loc));
  SmallVector<SILValue, 16> argValues;
  return {builder.createBranch(getRegularLocation(b.loc),
    castToBasicBlock(destBlock), getSILValues(arguments, argValues))};
}
