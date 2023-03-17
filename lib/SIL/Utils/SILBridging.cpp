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
#include "swift/AST/Attr.h"
#include "swift/AST/SemanticAttrs.h"
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

/// Fills \p storage with all Values from the bridged \p values array.
ArrayRef<SILValue> swift::getSILValues(BridgedValueArray values,
                                       SmallVectorImpl<SILValue> &storage) {
  // The bridged array contains class existentials, which have a layout of two
  // words. The first word is the actual object. Pick the objects and store them
  // into storage.
  for (unsigned idx = 0; idx < values.count; ++idx) {
    storage.push_back(values.base[idx].value.getSILValue());
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
#define NON_VALUE_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE) \
    VALUE(ID, NAME)
#define ARGUMENT(ID, PARENT) \
    VALUE(ID, NAME)
#define SINGLE_VALUE_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE) \
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

std::string BridgedFunction::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getFunction()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

BridgedFunction::ArgumentConvention BridgedFunction::getBridged(SILArgumentConvention conv) const {
  switch (conv.Value) {
    case SILArgumentConvention::Indirect_Inout:
      return BridgedFunction::ArgumentConvention::Indirect_Inout;
    case SILArgumentConvention::Indirect_InoutAliasable:
      return BridgedFunction::ArgumentConvention::Indirect_InoutAliasable;
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return BridgedFunction::ArgumentConvention::Indirect_In_Guaranteed;
    case SILArgumentConvention::Indirect_In:
      return BridgedFunction::ArgumentConvention::Indirect_In;
    case SILArgumentConvention::Indirect_Out:
      return BridgedFunction::ArgumentConvention::Indirect_Out;
    case SILArgumentConvention::Direct_Unowned:
      return BridgedFunction::ArgumentConvention::Direct_Unowned;
    case SILArgumentConvention::Direct_Owned:
      return BridgedFunction::ArgumentConvention::Direct_Owned;
    case SILArgumentConvention::Direct_Guaranteed:
      return BridgedFunction::ArgumentConvention::Direct_Guaranteed;
    case SILArgumentConvention::Pack_Inout:
    case SILArgumentConvention::Pack_Out:
    case SILArgumentConvention::Pack_Guaranteed:
    case SILArgumentConvention::Pack_Owned:
      llvm_unreachable("cannot bridge variadic generics");
  }
}

//===----------------------------------------------------------------------===//
//                               SILBasicBlock
//===----------------------------------------------------------------------===//

std::string BridgedBasicBlock::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getBlock()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                                SILArgument
//===----------------------------------------------------------------------===//

BridgedBasicBlock SILArgument_getParent(BridgedArgument argument) {
  return {castToArgument(argument)->getParent()};
}

static BridgedFunction::ArgumentConvention bridgeArgumentConvention(SILArgumentConvention convention) {
  switch (convention) {
    case SILArgumentConvention::Indirect_Inout:
      return BridgedFunction::ArgumentConvention::Indirect_Inout;
    case SILArgumentConvention::Indirect_InoutAliasable:
      return BridgedFunction::ArgumentConvention::Indirect_InoutAliasable;
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return BridgedFunction::ArgumentConvention::Indirect_In_Guaranteed;
    case SILArgumentConvention::Indirect_In:
      return BridgedFunction::ArgumentConvention::Indirect_In;
    case SILArgumentConvention::Indirect_Out:
      return BridgedFunction::ArgumentConvention::Indirect_Out;
    case SILArgumentConvention::Direct_Unowned:
      return BridgedFunction::ArgumentConvention::Direct_Unowned;
    case SILArgumentConvention::Direct_Owned:
      return BridgedFunction::ArgumentConvention::Direct_Owned;
    case SILArgumentConvention::Direct_Guaranteed:
      return BridgedFunction::ArgumentConvention::Direct_Guaranteed;
    case SILArgumentConvention::Pack_Inout:
    case SILArgumentConvention::Pack_Out:
    case SILArgumentConvention::Pack_Guaranteed:
    case SILArgumentConvention::Pack_Owned:
      llvm_unreachable("cannot bridge variadic generics");
  }
}

BridgedFunction::ArgumentConvention SILArgument_getConvention(BridgedArgument argument) {
  auto *arg = castToArgument<SILFunctionArgument>(argument);
  return bridgeArgumentConvention(arg->getArgumentConvention());
}

//===----------------------------------------------------------------------===//
//                                SILValue
//===----------------------------------------------------------------------===//

std::string SILNode_debugDescription(BridgedNode node) {
  std::string str;
  llvm::raw_string_ostream os(str);
  castToSILNode(node)->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

BridgedValue::Kind BridgedValue::getKind() const {
  SILValue v = getSILValue();
  if (isa<SingleValueInstruction>(v)) {
    return BridgedValue::Kind::SingleValueInstruction;
  } else if (isa<SILArgument>(v)) {
    return BridgedValue::Kind::Argument;
  } else if (isa<MultipleValueInstructionResult>(v)) {
    return BridgedValue::Kind::MultipleValueInstructionResult;
  } else if (isa<SILUndef>(v)) {
    return BridgedValue::Kind::Undef;
  }
  llvm_unreachable("unknown SILValue");
}

//===----------------------------------------------------------------------===//
//                                SILLocation
//===----------------------------------------------------------------------===//

std::string SILLocation_debugDescription(swift::SILDebugLocation dloc) {
  std::string str;
  llvm::raw_string_ostream os(str);
  SILLocation loc = dloc.getLocation();
  loc.print(os);
#ifndef NDEBUG
  if (const SILDebugScope *scope = dloc.getScope()) {
    if (DeclContext *dc = loc.getAsDeclContext()) {
      os << ", scope=";
      scope->print(dc->getASTContext().SourceMgr, os, /*indent*/ 2);
    } else {
      os << ", scope=?";
    }
  }
#endif
  return str;
}

SILDebugLocation SILLocation_getAutogeneratedLocation(SILDebugLocation loc) {
  SILDebugLocation autoGenLoc(RegularLocation::getAutoGeneratedLocation(),
                              loc.getScope());
  return autoGenLoc;
}

bool SILLocation_equal(swift::SILDebugLocation lhs, swift::SILDebugLocation rhs) {
  return lhs.getLocation() == rhs.getLocation() && lhs.getScope() == rhs.getScope();
}

bool SILLocation_hasSameSourceLocation(swift::SILDebugLocation lhs, swift::SILDebugLocation rhs) {
  return lhs.getLocation().hasSameSourceLocation(rhs.getLocation()) &&
         lhs.getScope() == rhs.getScope();
}

//===----------------------------------------------------------------------===//
//                            SILGlobalVariable
//===----------------------------------------------------------------------===//

std::string BridgedGlobalVar::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getGlobal()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                            SILVTable
//===----------------------------------------------------------------------===//

std::string BridgedVTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  vTable->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

std::string BridgedVTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  entry->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                    SILVWitnessTable, SILDefaultWitnessTable
//===----------------------------------------------------------------------===//

std::string BridgedWitnessTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  entry->print(os, /*verbose=*/ false, PrintOptions::printSIL());
  str.pop_back(); // Remove trailing newline.
  return str;
}

std::string BridgedWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

std::string BridgedDefaultWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
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

bool SILInstruction_isDeleted(BridgedInstruction inst) {
  return castToInst(inst)->isDeleted();
}

BridgedOperandArray SILInstruction_getOperands(BridgedInstruction inst) {
  auto operands = castToInst(inst)->getAllOperands();
  return {{operands.data()}, (SwiftInt)operands.size()};
}

void SILInstruction_setOperand(BridgedInstruction inst, SwiftInt index,
                               BridgedValue value) {
  castToInst(inst)->setOperand((unsigned)index, value.getSILValue());
}

SILDebugLocation SILInstruction_getLocation(BridgedInstruction inst) {
  return castToInst(inst)->getDebugLocation();
}

BridgedMemoryBehavior SILInstruction_getMemBehavior(BridgedInstruction inst) {
  return (BridgedMemoryBehavior)castToInst(inst)->getMemoryBehavior();
}

bool SILInstruction_mayRelease(BridgedInstruction inst) {
  return castToInst(inst)->mayRelease();
}

bool SILInstruction_hasUnspecifiedSideEffects(BridgedInstruction inst) {
  return castToInst(inst)->mayHaveSideEffects();
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

BridgedSuccessorArray TermInst_getSuccessors(BridgedInstruction term) {
  auto successors = castToInst<TermInst>(term)->getSuccessors();
  return {{successors.data()}, (SwiftInt)successors.size()};
}

//===----------------------------------------------------------------------===//
//                            Instruction classes
//===----------------------------------------------------------------------===//

llvm::StringRef CondFailInst_getMessage(BridgedInstruction cfi) {
  return castToInst<CondFailInst>(cfi)->getMessage();
}

SwiftInt LoadInst_getLoadOwnership(BridgedInstruction load) {
  return (SwiftInt)castToInst<LoadInst>(load)->getOwnershipQualifier();
}

BuiltinValueKind BuiltinInst_getID(BridgedInstruction bi) {
  return castToInst<BuiltinInst>(bi)->getBuiltinInfo().ID;
}

SwiftInt AddressToPointerInst_needsStackProtection(BridgedInstruction atp) {
  return castToInst<AddressToPointerInst>(atp)->needsStackProtection() ? 1 : 0;
}

SwiftInt IndexAddrInst_needsStackProtection(BridgedInstruction ia) {
  return castToInst<IndexAddrInst>(ia)->needsStackProtection() ? 1 : 0;
}

BridgedGlobalVar GlobalAccessInst_getGlobal(BridgedInstruction globalInst) {
  return {castToInst<GlobalAccessInst>(globalInst)->getReferencedGlobal()};
}

BridgedFunction FunctionRefBaseInst_getReferencedFunction(BridgedInstruction fri) {
  return {castToInst<FunctionRefBaseInst>(fri)->getInitiallyReferencedFunction()};
}

llvm::APInt IntegerLiteralInst_getValue(BridgedInstruction ili) {
  return castToInst<IntegerLiteralInst>(ili)->getValue();
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

bool ApplyInst_getNonThrowing(BridgedInstruction ai) {
  return castToInst<ApplyInst>(ai)->isNonThrowing();
}

bool ApplyInst_getNonAsync(BridgedInstruction ai) {
  return castToInst<ApplyInst>(ai)->isNonAsync();
}

const swift::GenericSpecializationInformation * _Nullable
ApplyInst_getSpecializationInfo(BridgedInstruction ai) {
  return castToInst<ApplyInst>(ai)->getSpecializationInfo();
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

SILAccessKind BeginAccessInst_getAccessKind(BridgedInstruction beginAccess) {
  return castToInst<BeginAccessInst>(beginAccess)->getAccessKind();
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

SwiftInt KeyPathInst_getNumComponents(BridgedInstruction kpi) {
  if (KeyPathPattern *pattern = castToInst<KeyPathInst>(kpi)->getPattern()) {
    return (SwiftInt)pattern->getComponents().size();
  }
  return 0;
}

void KeyPathInst_getReferencedFunctions(BridgedInstruction kpi, SwiftInt componentIdx,
                                            KeyPathFunctionResults * _Nonnull results) {
  KeyPathPattern *pattern = castToInst<KeyPathInst>(kpi)->getPattern();
  const KeyPathPatternComponent &comp = pattern->getComponents()[componentIdx];
  results->numFunctions = 0;

  comp.visitReferencedFunctionsAndMethods([results](SILFunction *func) {
      assert(results->numFunctions < KeyPathFunctionResults::maxFunctions);
      results->functions[results->numFunctions++] = {func};
    }, [](SILDeclRef) {});
}

SubstitutionMap ApplySite_getSubstitutionMap(BridgedInstruction inst) {
  auto as = ApplySite(castToInst(inst));
  return as.getSubstitutionMap();
}

BridgedFunction::ArgumentConvention
ApplySite_getArgumentConvention(BridgedInstruction inst, SwiftInt calleeArgIdx) {
  auto as = ApplySite(castToInst(inst));
  auto conv = as.getSubstCalleeConv().getSILArgumentConvention(calleeArgIdx);
  return bridgeArgumentConvention(conv.Value);
}

SwiftInt ApplySite_getNumArguments(BridgedInstruction inst) {
  auto as = ApplySite(castToInst(inst));
  return as.getNumArguments();
}

SwiftInt FullApplySite_numIndirectResultArguments(BridgedInstruction inst) {
  auto fas = FullApplySite(castToInst(inst));
  return fas.getNumIndirectSILResults();

}

//===----------------------------------------------------------------------===//
//                                SILBuilder
//===----------------------------------------------------------------------===//

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedBuilder b, StringRef name,
          SILType operandType, SILType resultType,
          BridgedValueArray arguments) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SmallVector<SILValue, 16> argValues;
  return {builder.createBuiltinBinaryFunction(
      RegularLocation(b.loc.getLocation()), name, operandType,
      resultType, getSILValues(arguments, argValues))};
}

BridgedInstruction SILBuilder_createCondFail(BridgedBuilder b,
          BridgedValue condition, StringRef message) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCondFail(RegularLocation(b.loc.getLocation()),
    condition.getSILValue(), message)};
}

BridgedInstruction SILBuilder_createIntegerLiteral(BridgedBuilder b,
          SILType type, SwiftInt value) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createIntegerLiteral(RegularLocation(b.loc.getLocation()),
                                       type, value)};
}

BridgedInstruction SILBuilder_createAllocStack(BridgedBuilder b,
          SILType type, SwiftInt hasDynamicLifetime, SwiftInt isLexical,
          SwiftInt wasMoved) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createAllocStack(
      RegularLocation(b.loc.getLocation()), type, None,
      hasDynamicLifetime != 0, isLexical != 0, wasMoved != 0)};
}

BridgedInstruction SILBuilder_createDeallocStack(BridgedBuilder b,
          BridgedValue operand) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDeallocStack(RegularLocation(b.loc.getLocation()),
                                     operand.getSILValue())};
}

BridgedInstruction SILBuilder_createDeallocStackRef(BridgedBuilder b,
          BridgedValue operand) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDeallocStackRef(RegularLocation(b.loc.getLocation()),
                                        operand.getSILValue())};
}

BridgedInstruction
SILBuilder_createUncheckedRefCast(BridgedBuilder b,
                                  BridgedValue op,
                                  SILType type) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createUncheckedRefCast(RegularLocation(b.loc.getLocation()),
                                         op.getSILValue(), type)};
}

BridgedInstruction SILBuilder_createSetDeallocating(BridgedBuilder b,
                                 BridgedValue op, bool isAtomic) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createSetDeallocating(
      RegularLocation(b.loc.getLocation()), op.getSILValue(),
      isAtomic ? RefCountingInst::Atomicity::Atomic
               : RefCountingInst::Atomicity::NonAtomic)};
}

BridgedInstruction
SILBuilder_createFunctionRef(BridgedBuilder b,
                             BridgedFunction function) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createFunctionRef(RegularLocation(b.loc.getLocation()),
                                    function.getFunction())};
}

BridgedInstruction SILBuilder_createCopyValue(BridgedBuilder b,
                                              BridgedValue op) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCopyValue(RegularLocation(b.loc.getLocation()),
                                  op.getSILValue())};
}

BridgedInstruction SILBuilder_createCopyAddr(BridgedBuilder b,
          BridgedValue from, BridgedValue to,
          SwiftInt takeSource, SwiftInt initializeDest) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCopyAddr(RegularLocation(b.loc.getLocation()),
                                 from.getSILValue(), to.getSILValue(),
                                 IsTake_t(takeSource != 0),
                                 IsInitialization_t(initializeDest != 0))};
}

BridgedInstruction SILBuilder_createDestroyValue(BridgedBuilder b,
                                                 BridgedValue op) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDestroyValue(RegularLocation(b.loc.getLocation()),
                                     op.getSILValue())};
}

BridgedInstruction SILBuilder_createDebugStep(BridgedBuilder b) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDebugStep(RegularLocation(b.loc.getLocation()))};
}

BridgedInstruction SILBuilder_createApply(BridgedBuilder b,
                                          BridgedValue function,
                                          SubstitutionMap subMap,
                                          BridgedValueArray arguments,
                                          bool isNonThrowing, bool isNonAsync,
                                          const GenericSpecializationInformation * _Nullable specInfo) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SmallVector<SILValue, 16> argValues;
  ApplyOptions applyOpts;
  if (isNonThrowing) { applyOpts |= ApplyFlags::DoesNotThrow; }
  if (isNonAsync) { applyOpts |= ApplyFlags::DoesNotAwait; }

  return {builder.createApply(RegularLocation(b.loc.getLocation()),
                              function.getSILValue(), subMap,
                              getSILValues(arguments, argValues),
                              applyOpts, specInfo)};
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
          SILType resultType) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SILValue en = enumVal.getSILValue();
  return {builder.createUncheckedEnumData(
      RegularLocation(b.loc.getLocation()), enumVal.getSILValue(),
      getEnumElement(en->getType(), caseIdx), resultType)};
}

BridgedInstruction SILBuilder_createSwitchEnumInst(BridgedBuilder b,
                                          BridgedValue enumVal,
                                          OptionalBridgedBasicBlock defaultBlock,
                                          const void * _Nullable enumCases,
                                          SwiftInt numEnumCases) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  using BridgedCase = const std::pair<SwiftInt, BridgedBasicBlock>;
  ArrayRef<BridgedCase> cases(static_cast<BridgedCase *>(enumCases),
                              (unsigned)numEnumCases);
  llvm::SmallDenseMap<SwiftInt, EnumElementDecl *> mappedElements;
  SILValue en = enumVal.getSILValue();
  EnumDecl *enumDecl = en->getType().getEnumOrBoundGenericEnum();
  for (auto elemWithIndex : llvm::enumerate(enumDecl->getAllElements())) {
    mappedElements[elemWithIndex.index()] = elemWithIndex.value();
  }
  SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 16> convertedCases;
  for (auto c : cases) {
    assert(mappedElements.count(c.first) && "wrong enum element index");
    convertedCases.push_back({mappedElements[c.first], c.second.getBlock()});
  }
  return {builder.createSwitchEnum(RegularLocation(b.loc.getLocation()),
                                   enumVal.getSILValue(),
                                   defaultBlock.getBlock(), convertedCases)};
}

BridgedInstruction SILBuilder_createBranch(
          BridgedBuilder b, BridgedBasicBlock destBlock,
          BridgedValueArray arguments) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SmallVector<SILValue, 16> argValues;
  return {builder.createBranch(RegularLocation(b.loc.getLocation()),
                               destBlock.getBlock(),
                               getSILValues(arguments, argValues))};
}

BridgedInstruction SILBuilder_createUnreachable(BridgedBuilder b) {
  SILBuilder builder(castToInst(b.insertBefore), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createUnreachable(RegularLocation(b.loc.getLocation()))};
}
