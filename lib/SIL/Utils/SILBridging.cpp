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
#include "swift/Basic/Assertions.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
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
  if (nodeMetatypesInitialized && !metatype) {
    ABORT([&](auto &out) {
      out << "Instruction " << getSILInstructionName((SILInstructionKind)kind)
          << " not registered";
    });
  }
  return metatype;
}

//===----------------------------------------------------------------------===//
//                          Class registration
//===----------------------------------------------------------------------===//

static llvm::StringMap<SILNodeKind> valueNamesToKind;

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
      ABORT([&](auto &out) {
        out << "Unknown bridged node class " << className;
      });
    }
    className = prefixedName;
  }
  SILNodeKind kind = iter->second;
  SwiftMetatype existingTy = nodeMetatypes[(unsigned)kind];
  if (existingTy) {
    ABORT([&](auto &out) {
      out << "Double registration of class " << className;
    });
  }
  nodeMetatypes[(unsigned)kind] = metatype;
}

//===----------------------------------------------------------------------===//
//                                Test
//===----------------------------------------------------------------------===//

void registerFunctionTest(BridgedStringRef name, void *nativeSwiftInvocation) {
  swift::test::FunctionTest::createNativeSwiftFunctionTest(
      name.unbridged(), nativeSwiftInvocation);
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
static_assert((int)BridgedFunction::PerformanceConstraints::NoRuntime == (int)swift::PerformanceConstraints::NoRuntime);
static_assert((int)BridgedFunction::PerformanceConstraints::NoExistentials == (int)swift::PerformanceConstraints::NoExistentials);
static_assert((int)BridgedFunction::PerformanceConstraints::NoObjCBridging == (int)swift::PerformanceConstraints::NoObjCBridging);

static_assert((int)BridgedFunction::InlineStrategy::InlineDefault == (int)swift::InlineDefault);
static_assert((int)BridgedFunction::InlineStrategy::NoInline == (int)swift::NoInline);
static_assert((int)BridgedFunction::InlineStrategy::AlwaysInline == (int)swift::AlwaysInline);

static_assert((int)BridgedFunction::ThunkKind::IsNotThunk == (int)swift::IsNotThunk);
static_assert((int)BridgedFunction::ThunkKind::IsThunk == (int)swift::IsThunk);
static_assert((int)BridgedFunction::ThunkKind::IsReabstractionThunk == (int)swift::IsReabstractionThunk);
static_assert((int)BridgedFunction::ThunkKind::IsSignatureOptimizedThunk == (int)swift::IsSignatureOptimizedThunk);

BridgedOwnedString BridgedFunction::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getFunction()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

BridgedSubstitutionMap BridgedFunction::getMethodSubstitutions(BridgedSubstitutionMap contextSubstitutions,
                                                               BridgedCanType selfType) const {
  swift::SILFunction *f = getFunction();
  swift::GenericSignature genericSig = f->getLoweredFunctionType()->getInvocationGenericSignature();

  if (!genericSig || genericSig->areAllParamsConcrete())
    return swift::SubstitutionMap();

  SubstitutionMap contextSubs = contextSubstitutions.unbridged();
  if (selfType.unbridged() &&
      contextSubs.getGenericSignature().getGenericParams().size() + 1 == genericSig.getGenericParams().size()) {

    // If this is a default witness methods (`selfType` != nil) it has generic self type. In this case
    // the generic self parameter is at depth 0 and the actual generic parameters of the substitution map
    // are at depth + 1, e.g:
    // ```
    //     @convention(witness_method: P) <τ_0_0><τ_1_0 where τ_0_0 : GenClass<τ_1_0>.T>
    //                                       ^      ^
    //                                    self      params of substitution map at depth + 1
    // ```
    return swift::SubstitutionMap::get(genericSig,
      [&](SubstitutableType *type) -> Type {
        GenericTypeParamType *genericParam = cast<GenericTypeParamType>(type);
        // The self type is τ_0_0
        if (genericParam->getDepth() == 0 && genericParam->getIndex() == 0)
          return selfType.unbridged();

        // Lookup the substitution map types at depth - 1.
        auto *depthMinus1Param = GenericTypeParamType::getType(genericParam->getDepth() - 1,
                                                               genericParam->getIndex(),
                                                               genericParam->getASTContext());
        return swift::QuerySubstitutionMap{contextSubs}(depthMinus1Param);
      },
      swift::LookUpConformanceInModule());

  }
  return swift::SubstitutionMap::get(genericSig,
                                     swift::QuerySubstitutionMap{contextSubs},
                                     swift::LookUpConformanceInModule());
}

//===----------------------------------------------------------------------===//
//                               SILBasicBlock
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedBasicBlock::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

//===----------------------------------------------------------------------===//
//                                SILValue
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedValue::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getSILValue()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
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

bool BridgedValue::findPointerEscape() const {
  return swift::findPointerEscape(getSILValue());
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
//                                Linkage
//===----------------------------------------------------------------------===//

static_assert((int)BridgedLinkage::Public == (int)swift::SILLinkage::Public);
static_assert((int)BridgedLinkage::PublicNonABI == (int)swift::SILLinkage::PublicNonABI);
static_assert((int)BridgedLinkage::Package == (int)swift::SILLinkage::Package);
static_assert((int)BridgedLinkage::PackageNonABI == (int)swift::SILLinkage::PackageNonABI);
static_assert((int)BridgedLinkage::Hidden == (int)swift::SILLinkage::Hidden);
static_assert((int)BridgedLinkage::Shared == (int)swift::SILLinkage::Shared);
static_assert((int)BridgedLinkage::Private == (int)swift::SILLinkage::Private);
static_assert((int)BridgedLinkage::PublicExternal == (int)swift::SILLinkage::PublicExternal);
static_assert((int)BridgedLinkage::PackageExternal == (int)swift::SILLinkage::PackageExternal);
static_assert((int)BridgedLinkage::HiddenExternal == (int)swift::SILLinkage::HiddenExternal);

//===----------------------------------------------------------------------===//
//                                Operand
//===----------------------------------------------------------------------===//

void BridgedOperand::changeOwnership(BridgedValue::Ownership from, BridgedValue::Ownership to) const {
  swift::ForwardingOperand forwardingOp(op);
  assert(forwardingOp);
  forwardingOp.replaceOwnershipKind(BridgedValue::unbridge(from), BridgedValue::unbridge(to));
}

//===----------------------------------------------------------------------===//
//                            SILGlobalVariable
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedGlobalVar::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  getGlobal()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
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

bool BridgedGlobalVar::isConstValue() const {
  SILGlobalVariable *global = getGlobal();
  if (const auto &decl = global->getDecl())
    return decl->isConstValue();
  return false;
}

//===----------------------------------------------------------------------===//
//                            SILDeclRef
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedDeclRef::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged().print(os);
  return BridgedOwnedString(str);
}

//===----------------------------------------------------------------------===//
//                            SILVTable
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedVTableEntry) >= sizeof(swift::SILVTableEntry),
              "BridgedVTableEntry has wrong size");

static_assert((int)BridgedVTableEntry::Kind::Normal == (int)swift::SILVTableEntry::Normal);
static_assert((int)BridgedVTableEntry::Kind::Inherited == (int)swift::SILVTableEntry::Inherited);
static_assert((int)BridgedVTableEntry::Kind::Override == (int)swift::SILVTableEntry::Override);

BridgedOwnedString BridgedVTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  vTable->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

BridgedOwnedString BridgedVTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged().print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

//===----------------------------------------------------------------------===//
//                    SILVWitnessTable, SILDefaultWitnessTable
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedWitnessTableEntry) >= sizeof(swift::SILWitnessTable::Entry),
              "BridgedWitnessTableEntry has wrong size");

static_assert((int)BridgedWitnessTableEntry::Kind::invalid == (int)swift::SILWitnessTable::WitnessKind::Invalid);
static_assert((int)BridgedWitnessTableEntry::Kind::method == (int)swift::SILWitnessTable::WitnessKind::Method);
static_assert((int)BridgedWitnessTableEntry::Kind::associatedType == (int)swift::SILWitnessTable::WitnessKind::AssociatedType);
static_assert((int)BridgedWitnessTableEntry::Kind::associatedConformance == (int)swift::SILWitnessTable::WitnessKind::AssociatedConformance);
static_assert((int)BridgedWitnessTableEntry::Kind::baseProtocol == (int)swift::SILWitnessTable::WitnessKind::BaseProtocol);

BridgedOwnedString BridgedWitnessTableEntry::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged().print(os, /*verbose=*/ false, PrintOptions::printSIL());
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

BridgedOwnedString BridgedWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

BridgedOwnedString BridgedDefaultWitnessTable::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  table->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

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
  return BridgedOwnedString(str);
}

//===----------------------------------------------------------------------===//
//                               SILInstruction
//===----------------------------------------------------------------------===//

static_assert((int)BridgedMemoryBehavior::None == (int)swift::MemoryBehavior::None);
static_assert((int)BridgedMemoryBehavior::MayRead == (int)swift::MemoryBehavior::MayRead);
static_assert((int)BridgedMemoryBehavior::MayWrite == (int)swift::MemoryBehavior::MayWrite);
static_assert((int)BridgedMemoryBehavior::MayReadWrite == (int)swift::MemoryBehavior::MayReadWrite);
static_assert((int)BridgedMemoryBehavior::MayHaveSideEffects == (int)swift::MemoryBehavior::MayHaveSideEffects);

BridgedOwnedString BridgedInstruction::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

bool BridgedInstruction::mayAccessPointer() const {
  return ::mayAccessPointer(unbridged());
}

bool BridgedInstruction::mayLoadWeakOrUnowned() const {
  return ::mayLoadWeakOrUnowned(unbridged());
}

bool BridgedInstruction::maySynchronize() const {
  return ::maySynchronize(unbridged());
}

bool BridgedInstruction::mayBeDeinitBarrierNotConsideringSideEffects() const {
  return ::mayBeDeinitBarrierNotConsideringSideEffects(unbridged());
}

//===----------------------------------------------------------------------===//
//                               BridgedBuilder
//===----------------------------------------------------------------------===//

static llvm::SmallVector<std::pair<swift::EnumElementDecl *, swift::SILBasicBlock *>, 16>
convertCases(SILType enumTy, const void * _Nullable enumCases, SwiftInt numEnumCases) {
  using BridgedCase = const std::pair<SwiftInt, BridgedBasicBlock>;
  llvm::ArrayRef<BridgedCase> cases(static_cast<BridgedCase *>(enumCases),
                                    (unsigned)numEnumCases);
  llvm::SmallDenseMap<SwiftInt, swift::EnumElementDecl *> mappedElements;
  swift::EnumDecl *enumDecl = enumTy.getEnumOrBoundGenericEnum();
  for (auto elemWithIndex : llvm::enumerate(enumDecl->getAllElements())) {
    mappedElements[elemWithIndex.index()] = elemWithIndex.value();
  }
  llvm::SmallVector<std::pair<swift::EnumElementDecl *, swift::SILBasicBlock *>, 16> convertedCases;
  for (auto c : cases) {
    assert(mappedElements.count(c.first) && "wrong enum element index");
    convertedCases.push_back({mappedElements[c.first], c.second.unbridged()});
  }
  return convertedCases;
}

BridgedInstruction BridgedBuilder::createSwitchEnumInst(BridgedValue enumVal, OptionalBridgedBasicBlock defaultBlock,
                                        const void * _Nullable enumCases, SwiftInt numEnumCases) const {
  return {unbridged().createSwitchEnum(regularLoc(),
                                       enumVal.getSILValue(),
                                       defaultBlock.unbridged(),
                                       convertCases(enumVal.getSILValue()->getType(), enumCases, numEnumCases))};
}

BridgedInstruction BridgedBuilder::createSwitchEnumAddrInst(BridgedValue enumAddr,
                                                            OptionalBridgedBasicBlock defaultBlock,
                                                            const void * _Nullable enumCases,
                                                            SwiftInt numEnumCases) const {
  return {unbridged().createSwitchEnumAddr(regularLoc(),
                                           enumAddr.getSILValue(),
                                           defaultBlock.unbridged(),
                                           convertCases(enumAddr.getSILValue()->getType(), enumCases, numEnumCases))};
}
