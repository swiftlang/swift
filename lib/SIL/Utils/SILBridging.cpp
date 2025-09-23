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
#include "swift/SIL/SILContext.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/TypeSubstCloner.h"
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

void registerTest(BridgedStringRef name, void *nativeSwiftContext) {
  swift::test::FunctionTest::createNativeSwiftFunctionTest(
      name.unbridged(), nativeSwiftContext, /*isSILTest=*/ true);
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
static_assert((int)BridgedFunction::PerformanceConstraints::ManualOwnership == (int)swift::PerformanceConstraints::ManualOwnership);

static_assert((int)BridgedFunction::InlineStrategy::InlineDefault == (int)swift::InlineDefault);
static_assert((int)BridgedFunction::InlineStrategy::NoInline == (int)swift::NoInline);
static_assert((int)BridgedFunction::InlineStrategy::AlwaysInline == (int)swift::AlwaysInline);

static_assert((int)BridgedFunction::ABILanguage::Swift == (int)swift::SILFunctionLanguage::Swift);
static_assert((int)BridgedFunction::ABILanguage::C == (int)swift::SILFunctionLanguage::C);

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

  auto props = global->getModule().Types.getTypeProperties(
      global->getLoweredType(),
      TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(expansion));
  return props.isFixedABI();
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

//===----------------------------------------------------------------------===//
//                               BridgedCloner
//===----------------------------------------------------------------------===//

// Need to put the cloner Impl classes into namespace swift to forward reference it from SILBridging.h.
namespace swift {

class BridgedClonerImpl : public SILCloner<BridgedClonerImpl> {
  friend class SILInstructionVisitor<BridgedClonerImpl>;
  friend class SILCloner<BridgedClonerImpl>;

  bool hasFixedLocation;
  union {
    SILDebugLocation fixedLocation;
    ScopeCloner scopeCloner;
  };

  SILInstruction *result = nullptr;

public:
  BridgedClonerImpl(SILGlobalVariable *gVar)
    : SILCloner<BridgedClonerImpl>(gVar),
      hasFixedLocation(true),
      fixedLocation(ArtificialUnreachableLocation(), nullptr) {}

  BridgedClonerImpl(SILInstruction *insertionPoint)
    : SILCloner<BridgedClonerImpl>(*insertionPoint->getFunction()),
      hasFixedLocation(true),
      fixedLocation(insertionPoint->getDebugLocation()) {
    Builder.setInsertionPoint(insertionPoint);
  }

  BridgedClonerImpl(SILFunction &emptyFunction)
    : SILCloner<BridgedClonerImpl>(emptyFunction),
      hasFixedLocation(false),
      scopeCloner(ScopeCloner(emptyFunction)) {}

  ~BridgedClonerImpl() {
    if (hasFixedLocation) {
      fixedLocation.~SILDebugLocation();
    } else {
      scopeCloner.~ScopeCloner();
    }
  }

  SILValue getClonedValue(SILValue v) {
    return getMappedValue(v);
  }

  SILInstruction *cloneInst(SILInstruction *inst) {
    result = nullptr;
    visit(inst);
    ASSERT(result && "instruction not cloned");
    return result;
  }

  SILLocation remapLocation(SILLocation loc) {
    if (hasFixedLocation)
      return fixedLocation.getLocation();
    return loc;
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (hasFixedLocation)
      return fixedLocation.getScope();
    return scopeCloner.getOrCreateClonedScope(DS);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    result = Cloned;
    SILCloner<BridgedClonerImpl>::postProcess(Orig, Cloned);
  }
};

class BridgedTypeSubstClonerImpl : public TypeSubstCloner<BridgedTypeSubstClonerImpl> {
  SILInstruction *result = nullptr;

public:
  BridgedTypeSubstClonerImpl(SILFunction &from, SILFunction &toEmptyFunction, SubstitutionMap subs)
    : TypeSubstCloner<BridgedTypeSubstClonerImpl>(toEmptyFunction, from, subs) {}

  SILValue getClonedValue(SILValue v) {
    return getMappedValue(v);
  }

  SILInstruction *cloneInst(SILInstruction *inst) {
    result = nullptr;
    visit(inst);
    ASSERT(result && "instruction not cloned");
    return result;
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    result = Cloned;
    SILClonerWithScopes<BridgedTypeSubstClonerImpl>::postProcess(Orig, Cloned);
  }

  SILFunction *getOriginal() { return &Original; }
};

} // namespace swift

BridgedCloner::BridgedCloner(BridgedGlobalVar var, BridgedContext context)
  : cloner(new BridgedClonerImpl(var.getGlobal())) {
  context.context->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedInstruction inst,
                             BridgedContext context)
    : cloner(new BridgedClonerImpl(inst.unbridged())) {
  context.context->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedFunction emptyFunction, BridgedContext context)
  : cloner(new BridgedClonerImpl(*emptyFunction.getFunction())) {
  context.context->notifyNewCloner();
}

void BridgedCloner::destroy(BridgedContext context) {
  delete cloner;
  cloner = nullptr;
  context.context->notifyClonerDestroyed();
}

BridgedFunction BridgedCloner::getCloned() const {
  return { &cloner->getBuilder().getFunction() };
}

BridgedValue BridgedCloner::getClonedValue(BridgedValue v) {
  return {cloner->getClonedValue(v.getSILValue())};
}

bool BridgedCloner::isValueCloned(BridgedValue v) const {
  return cloner->isValueCloned(v.getSILValue());
}

void BridgedCloner::recordClonedInstruction(BridgedInstruction origInst, BridgedInstruction clonedInst) const {
  cloner->recordClonedInstruction(origInst.unbridged(), clonedInst.unbridged());
}

void BridgedCloner::recordFoldedValue(BridgedValue orig, BridgedValue mapped) const {
  cloner->recordFoldedValue(orig.getSILValue(), mapped.getSILValue());
}

BridgedInstruction BridgedCloner::clone(BridgedInstruction inst) const {
  return {cloner->cloneInst(inst.unbridged())->asSILNode()};
}

BridgedBasicBlock BridgedCloner::getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const {
  return { cloner->getOpBasicBlock(originalBasicBlock.unbridged()) };
}

void BridgedCloner::cloneFunctionBody(BridgedFunction originalFunction,
                                      BridgedBasicBlock clonedEntryBlock,
                                      BridgedValueArray clonedEntryBlockArgs) const {
  llvm::SmallVector<swift::SILValue, 16> clonedEntryBlockArgsStorage;
  auto clonedEntryBlockArgsArrayRef = clonedEntryBlockArgs.getValues(clonedEntryBlockArgsStorage);
  cloner->cloneFunctionBody(originalFunction.getFunction(), clonedEntryBlock.unbridged(), clonedEntryBlockArgsArrayRef);
}

void BridgedCloner::cloneFunctionBody(BridgedFunction originalFunction) const {
  cloner->cloneFunction(originalFunction.getFunction());
}

BridgedTypeSubstCloner::BridgedTypeSubstCloner(BridgedFunction fromFunction, BridgedFunction toFunction,
                                               BridgedSubstitutionMap substitutions,
                                               BridgedContext context)
  : cloner(new BridgedTypeSubstClonerImpl(*fromFunction.getFunction(), *toFunction.getFunction(),
                                          substitutions.unbridged())) {
  context.context->notifyNewCloner();
}

void BridgedTypeSubstCloner::destroy(BridgedContext context) {
  delete cloner;
  cloner = nullptr;
  context.context->notifyClonerDestroyed();
}

void BridgedTypeSubstCloner::cloneFunctionBody() const {
  cloner->cloneFunction(cloner->getOriginal());
}

BridgedBasicBlock BridgedTypeSubstCloner::getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const {
  return { cloner->getOpBasicBlock(originalBasicBlock.unbridged()) };
}

BridgedValue BridgedTypeSubstCloner::getClonedValue(BridgedValue v) {
  return {cloner->getClonedValue(v.getSILValue())};
}

//===----------------------------------------------------------------------===//
//                               BridgedContext
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedContext::getModuleDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  context->getModule()->print(os);
  str.pop_back(); // Remove trailing newline.
  return BridgedOwnedString(str);
}

OptionalBridgedFunction BridgedContext::lookUpNominalDeinitFunction(BridgedDeclObj nominal)  const {
  return {context->getModule()->lookUpMoveOnlyDeinitFunction(nominal.getAs<swift::NominalTypeDecl>())};
}

BridgedFunction BridgedContext::
createEmptyFunction(BridgedStringRef name,
                    const BridgedParameterInfo * _Nullable bridgedParams,
                    SwiftInt paramCount,
                    bool hasSelfParam,
                    BridgedFunction fromFunc) const {
  llvm::SmallVector<SILParameterInfo> params;
  for (unsigned idx = 0; idx < paramCount; ++idx) {
    params.push_back(bridgedParams[idx].unbridged());
  }
  return {context->createEmptyFunction(name.unbridged(), params, hasSelfParam, fromFunc.getFunction())};
}

BridgedGlobalVar BridgedContext::createGlobalVariable(BridgedStringRef name, BridgedType type,
                                                      BridgedLinkage linkage,
                                                      bool isLet,
                                                      bool markedAsUsed) const {
  auto *global = SILGlobalVariable::create(
      *context->getModule(),
      (swift::SILLinkage)linkage, IsNotSerialized,
      name.unbridged(), type.unbridged());
  if (isLet)
    global->setLet(true);
  global->setMarkedAsUsed(markedAsUsed);
  return {global};
}

void BridgedContext::moveFunctionBody(BridgedFunction sourceFunc, BridgedFunction destFunc) const {
  context->moveFunctionBody(sourceFunc.getFunction(), destFunc.getFunction());
}

//===----------------------------------------------------------------------===//
//                           SILContext
//===----------------------------------------------------------------------===//

SILContext::~SILContext() {}

void SILContext::verifyEverythingIsCleared() {
  ASSERT(allocatedSlabs.empty() && "StackList is leaking slabs");
  ASSERT(numBlockSetsAllocated == 0 && "Not all BasicBlockSets deallocated");
  ASSERT(numNodeSetsAllocated == 0 && "Not all NodeSets deallocated");
  ASSERT(numOperandSetsAllocated == 0 && "Not all OperandSets deallocated");
  ASSERT(numClonersAllocated == 0 && "Not all cloners deallocated");
}

FixedSizeSlab *SILContext::allocSlab(FixedSizeSlab *afterSlab) {
  FixedSizeSlab *slab = getModule()->allocSlab();
  if (afterSlab) {
    allocatedSlabs.insert(std::next(afterSlab->getIterator()), *slab);
  } else {
    allocatedSlabs.push_back(*slab);
  }
  return slab;
}

FixedSizeSlab *SILContext::freeSlab(FixedSizeSlab *slab) {
  FixedSizeSlab *prev = nullptr;
  assert(!allocatedSlabs.empty());
  if (&allocatedSlabs.front() != slab)
    prev = &*std::prev(slab->getIterator());

  allocatedSlabs.remove(*slab);
  getModule()->freeSlab(slab);
  return prev;
}

BasicBlockSet *SILContext::allocBlockSet() {
  ASSERT(numBlockSetsAllocated < BlockSetCapacity &&
         "too many BasicBlockSets allocated");

  auto *storage = (BasicBlockSet *)blockSetStorage + numBlockSetsAllocated;
  BasicBlockSet *set = new (storage) BasicBlockSet(function);
  aliveBlockSets[numBlockSetsAllocated] = true;
  ++numBlockSetsAllocated;
  return set;
}

void SILContext::freeBlockSet(BasicBlockSet *set) {
  int idx = set - (BasicBlockSet *)blockSetStorage;
  assert(idx >= 0 && idx < numBlockSetsAllocated);
  assert(aliveBlockSets[idx] && "double free of BasicBlockSet");
  aliveBlockSets[idx] = false;

  while (numBlockSetsAllocated > 0 && !aliveBlockSets[numBlockSetsAllocated - 1]) {
    auto *set = (BasicBlockSet *)blockSetStorage + numBlockSetsAllocated - 1;
    set->~BasicBlockSet();
    --numBlockSetsAllocated;
  }
}

NodeSet *SILContext::allocNodeSet() {
  ASSERT(numNodeSetsAllocated < NodeSetCapacity &&
         "too many NodeSets allocated");

  auto *storage = (NodeSet *)nodeSetStorage + numNodeSetsAllocated;
  NodeSet *set = new (storage) NodeSet(function);
  aliveNodeSets[numNodeSetsAllocated] = true;
  ++numNodeSetsAllocated;
  return set;
}

void SILContext::freeNodeSet(NodeSet *set) {
  int idx = set - (NodeSet *)nodeSetStorage;
  assert(idx >= 0 && idx < numNodeSetsAllocated);
  assert(aliveNodeSets[idx] && "double free of NodeSet");
  aliveNodeSets[idx] = false;

  while (numNodeSetsAllocated > 0 && !aliveNodeSets[numNodeSetsAllocated - 1]) {
    auto *set = (NodeSet *)nodeSetStorage + numNodeSetsAllocated - 1;
    set->~NodeSet();
    --numNodeSetsAllocated;
  }
}

OperandSet *SILContext::allocOperandSet() {
  ASSERT(numOperandSetsAllocated < OperandSetCapacity &&
         "too many OperandSets allocated");

  auto *storage = (OperandSet *)operandSetStorage + numOperandSetsAllocated;
  OperandSet *set = new (storage) OperandSet(function);
  aliveOperandSets[numOperandSetsAllocated] = true;
  ++numOperandSetsAllocated;
  return set;
}

void SILContext::freeOperandSet(OperandSet *set) {
  int idx = set - (OperandSet *)operandSetStorage;
  assert(idx >= 0 && idx < numOperandSetsAllocated);
  assert(aliveOperandSets[idx] && "double free of OperandSet");
  aliveOperandSets[idx] = false;

  while (numOperandSetsAllocated > 0 && !aliveOperandSets[numOperandSetsAllocated - 1]) {
    auto *set = (OperandSet *)operandSetStorage + numOperandSetsAllocated - 1;
    set->~OperandSet();
    --numOperandSetsAllocated;
  }
}

//===----------------------------------------------------------------------===//
//                           BridgedVerifier
//===----------------------------------------------------------------------===//

static BridgedVerifier::VerifyFunctionFn verifyFunctionFunction = nullptr;

void BridgedVerifier::registerVerifier(VerifyFunctionFn verifyFunctionFn) {
  verifyFunctionFunction = verifyFunctionFn;
}

void BridgedVerifier::runSwiftFunctionVerification(SILFunction * _Nonnull f, SILContext * _Nonnull context) {
  if (!verifyFunctionFunction)
    return;

  verifyFunctionFunction({context}, {f});
}

void BridgedVerifier::verifierError(BridgedStringRef message,
                                    OptionalBridgedInstruction atInstruction,
                                    OptionalBridgedArgument atArgument) {
  Twine msg(message.unbridged());
  verificationFailure(msg, atInstruction.unbridged(), atArgument.unbridged(), /*extraContext=*/nullptr);
}
