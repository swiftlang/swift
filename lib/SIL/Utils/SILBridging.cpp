//===--- SILBridging.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBridging.h"

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined and therefore inluded in the cpp file.
#include "swift/SIL/SILBridgingImpl.h"
#endif

#include "swift/AST/Attr.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/ParseTestSpecification.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILNode.h"
#include "swift/SIL/Test.h"
#include <string>
#include <cstring>
#include <stdio.h>

using namespace swift;

namespace {

bool nodeMetatypesInitialized = false;

// Filled in by class registration in initializeSwiftModules().
SwiftMetatype nodeMetatypes[(unsigned)SILNodeKind::Last_SILNode + 1];

}

bool swiftModulesInitialized() {
  return nodeMetatypesInitialized;
}

// Does return null if initializeSwiftModules() is never called.
SwiftMetatype SILNode::getSILNodeMetatype(SILNodeKind kind) {
  SwiftMetatype metatype = nodeMetatypes[(unsigned)kind];
  assert((!nodeMetatypesInitialized || metatype) &&
        "no metatype for bridged SIL node");
  return metatype;
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
void registerBridgedClass(BridgedStringRef bridgedClassName, SwiftMetatype metatype) {
  StringRef className = bridgedClassName.unbridged();
  nodeMetatypesInitialized = true;

  // Handle the important non Node classes.
  if (className == "BasicBlock")
    return SILBasicBlock::registerBridgedMetatype(metatype);
  if (className == "GlobalVariable")
    return SILGlobalVariable::registerBridgedMetatype(metatype);
  if (className == "Argument") {
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
//                                Test
//===----------------------------------------------------------------------===//

void registerFunctionTest(BridgedStringRef name, void *nativeSwiftInvocation) {
  new swift::test::FunctionTest(name.unbridged(), nativeSwiftInvocation);
}

bool BridgedTestArguments::hasUntaken() const {
  return arguments->hasUntaken();
}

BridgedStringRef BridgedTestArguments::takeString() const {
  return arguments->takeString();
}

bool BridgedTestArguments::takeBool() const { return arguments->takeBool(); }

SwiftInt BridgedTestArguments::takeInt() const { return arguments->takeUInt(); }

BridgedOperand BridgedTestArguments::takeOperand() const {
  return {arguments->takeOperand()};
}

BridgedValue BridgedTestArguments::takeValue() const {
  return {arguments->takeValue()};
}

BridgedInstruction BridgedTestArguments::takeInstruction() const {
  return {arguments->takeInstruction()->asSILNode()};
}

BridgedArgument BridgedTestArguments::takeArgument() const {
  return {arguments->takeBlockArgument()};
}

BridgedBasicBlock BridgedTestArguments::takeBlock() const {
  return {arguments->takeBlock()};
}

BridgedFunction BridgedTestArguments::takeFunction() const {
  return {arguments->takeFunction()};
}

//===----------------------------------------------------------------------===//
//                                SILType
//===----------------------------------------------------------------------===//

static_assert((int)BridgedType::MetatypeRepresentation::Thin == (int)swift::MetatypeRepresentation::Thin);
static_assert((int)BridgedType::MetatypeRepresentation::Thick == (int)swift::MetatypeRepresentation::Thick);
static_assert((int)BridgedType::MetatypeRepresentation::ObjC == (int)swift::MetatypeRepresentation::ObjC);

static_assert((int)BridgedType::TraitResult::IsNot == (int)swift::TypeTraitResult::IsNot);
static_assert((int)BridgedType::TraitResult::CanBe == (int)swift::TypeTraitResult::CanBe);
static_assert((int)BridgedType::TraitResult::Is == (int)swift::TypeTraitResult::Is);


//===----------------------------------------------------------------------===//
//                                SILFunction
//===----------------------------------------------------------------------===//

static_assert((int)BridgedFunction::EffectsKind::ReadNone == (int)swift::EffectsKind::ReadNone);
static_assert((int)BridgedFunction::EffectsKind::ReadOnly == (int)swift::EffectsKind::ReadOnly);
static_assert((int)BridgedFunction::EffectsKind::ReleaseNone == (int)swift::EffectsKind::ReleaseNone);
static_assert((int)BridgedFunction::EffectsKind::ReadWrite == (int)swift::EffectsKind::ReadWrite);
static_assert((int)BridgedFunction::EffectsKind::Unspecified == (int)swift::EffectsKind::Unspecified);
static_assert((int)BridgedFunction::EffectsKind::Custom == (int)swift::EffectsKind::Custom);

static_assert((int)BridgedFunction::PerformanceConstraints::None == (int)swift::PerformanceConstraints::None);
static_assert((int)BridgedFunction::PerformanceConstraints::NoAllocation == (int)swift::PerformanceConstraints::NoAllocation);
static_assert((int)BridgedFunction::PerformanceConstraints::NoLocks == (int)swift::PerformanceConstraints::NoLocks);

static_assert((int)BridgedFunction::InlineStrategy::InlineDefault == (int)swift::InlineDefault);
static_assert((int)BridgedFunction::InlineStrategy::NoInline == (int)swift::NoInline);
static_assert((int)BridgedFunction::InlineStrategy::AlwaysInline == (int)swift::AlwaysInline);

BridgedOwnedString BridgedFunction::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getFunction()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                               SILBasicBlock
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedBasicBlock::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                                SILValue
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedValue::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getSILValue()->print(os);
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

ArrayRef<SILValue> BridgedValueArray::getValues(SmallVectorImpl<SILValue> &storage) {
  for (unsigned idx = 0; idx < count; ++idx) {
    storage.push_back(base[idx].value.getSILValue());
  }
  return storage;
}


//===----------------------------------------------------------------------===//
//                                SILArgument
//===----------------------------------------------------------------------===//

static_assert((int)BridgedArgumentConvention::Indirect_In == (int)swift::SILArgumentConvention::Indirect_In);
static_assert((int)BridgedArgumentConvention::Indirect_In_Guaranteed == (int)swift::SILArgumentConvention::Indirect_In_Guaranteed);
static_assert((int)BridgedArgumentConvention::Indirect_Inout == (int)swift::SILArgumentConvention::Indirect_Inout);
static_assert((int)BridgedArgumentConvention::Indirect_InoutAliasable == (int)swift::SILArgumentConvention::Indirect_InoutAliasable);
static_assert((int)BridgedArgumentConvention::Indirect_Out == (int)swift::SILArgumentConvention::Indirect_Out);
static_assert((int)BridgedArgumentConvention::Direct_Owned == (int)swift::SILArgumentConvention::Direct_Owned);
static_assert((int)BridgedArgumentConvention::Direct_Unowned == (int)swift::SILArgumentConvention::Direct_Unowned);
static_assert((int)BridgedArgumentConvention::Direct_Guaranteed == (int)swift::SILArgumentConvention::Direct_Guaranteed);
static_assert((int)BridgedArgumentConvention::Pack_Owned == (int)swift::SILArgumentConvention::Pack_Owned);
static_assert((int)BridgedArgumentConvention::Pack_Inout == (int)swift::SILArgumentConvention::Pack_Inout);
static_assert((int)BridgedArgumentConvention::Pack_Guaranteed == (int)swift::SILArgumentConvention::Pack_Guaranteed);
static_assert((int)BridgedArgumentConvention::Pack_Out == (int)swift::SILArgumentConvention::Pack_Out);


//===----------------------------------------------------------------------===//
//                            SILGlobalVariable
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedGlobalVar::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getGlobal()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

bool BridgedGlobalVar::canBeInitializedStatically() const {
  SILGlobalVariable *global = getGlobal();
  auto expansion = ResilienceExpansion::Maximal;
  if (hasPublicVisibility(global->getLinkage()))
    expansion = ResilienceExpansion::Minimal;

  auto &tl = global->getModule().Types.getTypeLowering(
      global->getLoweredType(),
      TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(expansion));
  return tl.isLoadable();
}

bool BridgedGlobalVar::mustBeInitializedStatically() const {
  SILGlobalVariable *global = getGlobal();
  return global->mustBeInitializedStatically();
}

//===----------------------------------------------------------------------===//
//                            SILVTable
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedVTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  vTable->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

BridgedOwnedString BridgedVTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  entry->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                    SILVWitnessTable, SILDefaultWitnessTable
//===----------------------------------------------------------------------===//

static_assert((int)BridgedWitnessTableEntry::Kind::Invalid == (int)swift::SILWitnessTable::WitnessKind::Invalid);
static_assert((int)BridgedWitnessTableEntry::Kind::Method == (int)swift::SILWitnessTable::WitnessKind::Method);
static_assert((int)BridgedWitnessTableEntry::Kind::AssociatedType == (int)swift::SILWitnessTable::WitnessKind::AssociatedType);
static_assert((int)BridgedWitnessTableEntry::Kind::AssociatedTypeProtocol == (int)swift::SILWitnessTable::WitnessKind::AssociatedTypeProtocol);
static_assert((int)BridgedWitnessTableEntry::Kind::BaseProtocol == (int)swift::SILWitnessTable::WitnessKind::BaseProtocol);

BridgedOwnedString BridgedWitnessTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getEntry()->print(os, /*verbose=*/ false, PrintOptions::printSIL());
  str.pop_back(); // Remove trailing newline.
  return str;
}

BridgedOwnedString BridgedWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

BridgedOwnedString BridgedDefaultWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

//===----------------------------------------------------------------------===//
//                               SubstitutionMap
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedSubstitutionMap) >= sizeof(swift::SubstitutionMap),
              "BridgedSubstitutionMap has wrong size");


//===----------------------------------------------------------------------===//
//                               SILDebugLocation
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedLocation) >= sizeof(swift::SILDebugLocation),
              "BridgedLocation has wrong size");

BridgedOwnedString BridgedLocation::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  SILLocation loc = getLoc().getLocation();
  loc.print(os);
#ifndef NDEBUG
  if (const SILDebugScope *scope = getLoc().getScope()) {
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

//===----------------------------------------------------------------------===//
//                               SILInstruction
//===----------------------------------------------------------------------===//

static_assert((int)BridgedMemoryBehavior::None == (int)swift::MemoryBehavior::None);
static_assert((int)BridgedMemoryBehavior::MayRead == (int)swift::MemoryBehavior::MayRead);
static_assert((int)BridgedMemoryBehavior::MayWrite == (int)swift::MemoryBehavior::MayWrite);
static_assert((int)BridgedMemoryBehavior::MayReadWrite == (int)swift::MemoryBehavior::MayReadWrite);
static_assert((int)BridgedMemoryBehavior::MayHaveSideEffects == (int)swift::MemoryBehavior::MayHaveSideEffects);

static_assert((int)BridgedInstruction::AccessKind::Init == (int)swift::SILAccessKind::Init);
static_assert((int)BridgedInstruction::AccessKind::Read == (int)swift::SILAccessKind::Read);
static_assert((int)BridgedInstruction::AccessKind::Modify == (int)swift::SILAccessKind::Modify);
static_assert((int)BridgedInstruction::AccessKind::Deinit == (int)swift::SILAccessKind::Deinit);

BridgedOwnedString BridgedInstruction::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

bool BridgedInstruction::mayAccessPointer() const {
  return ::mayAccessPointer(unbridged());
}

bool BridgedInstruction::mayLoadWeakOrUnowned() const {
  return ::mayLoadWeakOrUnowned(unbridged());
}

bool BridgedInstruction::maySynchronizeNotConsideringSideEffects() const {
  return ::maySynchronizeNotConsideringSideEffects(unbridged());
}

bool BridgedInstruction::mayBeDeinitBarrierNotConsideringSideEffects() const {
  return ::mayBeDeinitBarrierNotConsideringSideEffects(unbridged());
}

void writeCharToStderr(int c) {
  putc(c, stderr);
}
