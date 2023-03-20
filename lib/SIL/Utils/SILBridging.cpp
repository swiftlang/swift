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
#include "swift/SIL/MemAccessUtils.h"
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

bool BridgedInstruction::mayAccessPointer() const {
  return ::mayAccessPointer(getInst());
}

bool BridgedInstruction::mayLoadWeakOrUnowned() const {
  return ::mayLoadWeakOrUnowned(getInst());
}

bool BridgedInstruction::maySynchronizeNotConsideringSideEffects() const {
  return ::maySynchronizeNotConsideringSideEffects(getInst());
}

bool BridgedInstruction::mayBeDeinitBarrierNotConsideringSideEffects() const {
  return ::mayBeDeinitBarrierNotConsideringSideEffects(getInst());
}

//===----------------------------------------------------------------------===//
//                                SILBuilder
//===----------------------------------------------------------------------===//

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedBuilder b, StringRef name,
          SILType operandType, SILType resultType,
          BridgedValueArray arguments) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SmallVector<SILValue, 16> argValues;
  return {builder.createBuiltinBinaryFunction(
      RegularLocation(b.loc.getLocation()), name, operandType,
      resultType, getSILValues(arguments, argValues))};
}

BridgedInstruction SILBuilder_createCondFail(BridgedBuilder b,
          BridgedValue condition, StringRef message) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCondFail(RegularLocation(b.loc.getLocation()),
    condition.getSILValue(), message)};
}

BridgedInstruction SILBuilder_createIntegerLiteral(BridgedBuilder b,
          SILType type, SwiftInt value) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createIntegerLiteral(RegularLocation(b.loc.getLocation()),
                                       type, value)};
}

BridgedInstruction SILBuilder_createAllocStack(BridgedBuilder b,
          SILType type, SwiftInt hasDynamicLifetime, SwiftInt isLexical,
          SwiftInt wasMoved) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createAllocStack(
      RegularLocation(b.loc.getLocation()), type, None,
      hasDynamicLifetime != 0, isLexical != 0, wasMoved != 0)};
}

BridgedInstruction SILBuilder_createDeallocStack(BridgedBuilder b,
          BridgedValue operand) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDeallocStack(RegularLocation(b.loc.getLocation()),
                                     operand.getSILValue())};
}

BridgedInstruction SILBuilder_createDeallocStackRef(BridgedBuilder b,
          BridgedValue operand) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDeallocStackRef(RegularLocation(b.loc.getLocation()),
                                        operand.getSILValue())};
}

BridgedInstruction
SILBuilder_createUncheckedRefCast(BridgedBuilder b,
                                  BridgedValue op,
                                  SILType type) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createUncheckedRefCast(RegularLocation(b.loc.getLocation()),
                                         op.getSILValue(), type)};
}

BridgedInstruction SILBuilder_createSetDeallocating(BridgedBuilder b,
                                 BridgedValue op, bool isAtomic) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createSetDeallocating(
      RegularLocation(b.loc.getLocation()), op.getSILValue(),
      isAtomic ? RefCountingInst::Atomicity::Atomic
               : RefCountingInst::Atomicity::NonAtomic)};
}

BridgedInstruction
SILBuilder_createFunctionRef(BridgedBuilder b,
                             BridgedFunction function) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createFunctionRef(RegularLocation(b.loc.getLocation()),
                                    function.getFunction())};
}

BridgedInstruction SILBuilder_createCopyValue(BridgedBuilder b,
                                              BridgedValue op) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCopyValue(RegularLocation(b.loc.getLocation()),
                                  op.getSILValue())};
}

BridgedInstruction SILBuilder_createCopyAddr(BridgedBuilder b,
          BridgedValue from, BridgedValue to,
          SwiftInt takeSource, SwiftInt initializeDest) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createCopyAddr(RegularLocation(b.loc.getLocation()),
                                 from.getSILValue(), to.getSILValue(),
                                 IsTake_t(takeSource != 0),
                                 IsInitialization_t(initializeDest != 0))};
}

BridgedInstruction SILBuilder_createDestroyValue(BridgedBuilder b,
                                                 BridgedValue op) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDestroyValue(RegularLocation(b.loc.getLocation()),
                                     op.getSILValue())};
}

BridgedInstruction SILBuilder_createDebugStep(BridgedBuilder b) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createDebugStep(RegularLocation(b.loc.getLocation()))};
}

BridgedInstruction SILBuilder_createApply(BridgedBuilder b,
                                          BridgedValue function,
                                          SubstitutionMap subMap,
                                          BridgedValueArray arguments,
                                          bool isNonThrowing, bool isNonAsync,
                                          const GenericSpecializationInformation * _Nullable specInfo) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
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
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
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
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
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
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  SmallVector<SILValue, 16> argValues;
  return {builder.createBranch(RegularLocation(b.loc.getLocation()),
                               destBlock.getBlock(),
                               getSILValues(arguments, argValues))};
}

BridgedInstruction SILBuilder_createUnreachable(BridgedBuilder b) {
  SILBuilder builder(b.insertBefore.getInst(), b.insertAtEnd.getBlock(),
                     b.loc.getScope());
  return {builder.createUnreachable(RegularLocation(b.loc.getLocation()))};
}
