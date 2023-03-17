//===--- SILBridging.h - header for the swift SILBridging module ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBRIDGING_H
#define SWIFT_SIL_SILBRIDGING_H

#include "swift/Basic/BasicBridging.h"
#include "swift/Basic/BridgedSwiftObject.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILVTable.h"
#include <stdbool.h>
#include <stddef.h>
#include <string>

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct BridgedInstruction;
struct OptionalBridgedOperand;
struct OptionalBridgedSuccessor;
struct OptionalBridgedBasicBlock;

enum ChangeNotificationKind {
  instructionsChanged,
  callsChanged,
  branchesChanged,
  effectsChanged
};

typedef struct {
  const void * _Nonnull opaqueCtxt;
} BridgedPassContext;

struct BridgedValue {
  SwiftObject obj;

  enum class Kind {
    SingleValueInstruction,
    Argument,
    MultipleValueInstructionResult,
    Undef
  };

  // Unfortunately we need to take a detour over this enum.
  // Currently it's not possible to switch over `OwnershipKind::inntery`, because it's not a class enum.
  enum class Ownership {
    Unowned,
    Owned,
    Guaranteed,
    None
  };

  Kind getKind() const;

  swift::SILValue getSILValue() const { return static_cast<swift::ValueBase *>(obj); }

  SWIFT_IMPORT_UNSAFE
  inline OptionalBridgedOperand getFirstUse() const;

  SWIFT_IMPORT_UNSAFE
  swift::SILType getType() const { return getSILValue()->getType(); }

  Ownership getOwnership() const {
    switch (getSILValue()->getOwnershipKind()) {
      case swift::OwnershipKind::Any:
        llvm_unreachable("Invalid ownership for value");
      case swift::OwnershipKind::Unowned:    return Ownership::Unowned;
      case swift::OwnershipKind::Owned:      return Ownership::Owned;
      case swift::OwnershipKind::Guaranteed: return Ownership::Guaranteed;
      case swift::OwnershipKind::None:       return Ownership::None;
    }
  }
};

// This is the layout of a class existential.
struct BridgeValueExistential {
  BridgedValue value;
  void * _Nonnull conformance;
};

typedef struct {
  const BridgeValueExistential * _Nullable base;
  size_t count;
} BridgedValueArray;

struct BridgedOperand {
  swift::Operand * _Nonnull op;

  bool isTypeDependent() const { return op->isTypeDependent(); }

  SWIFT_IMPORT_UNSAFE
  inline OptionalBridgedOperand getNextUse() const;

  SWIFT_IMPORT_UNSAFE
  BridgedValue getValue() const { return {op->get()}; }

  SWIFT_IMPORT_UNSAFE
  inline BridgedInstruction getUser() const;
};

struct OptionalBridgedOperand {
  swift::Operand * _Nullable op;

  // Assumes that `op` is not null.
  SWIFT_IMPORT_UNSAFE
  BridgedOperand advancedBy(SwiftInt index) const { return {op + index}; }

  // Assumes that `op` is not null.
  SwiftInt distanceTo(BridgedOperand element) const { return element.op - op; }
};

struct BridgedOperandArray {
  OptionalBridgedOperand base;
  SwiftInt count;
};

// Must be in sync with SILInstruction::MemoryBehavior
// TODO: do this less hacky.
typedef enum {
  NoneBehavior,
  MayReadBehavior,
  MayWriteBehavior,
  MayReadWriteBehavior,
  MayHaveSideEffectsBehavior
} BridgedMemoryBehavior;

struct BridgedFunction {
  SwiftObject obj;

  SWIFT_IMPORT_UNSAFE
  swift::SILFunction * _Nonnull getFunction() const {
    return static_cast<swift::SILFunction *>(obj);
  }

  // Unfortunately we need to take a detour over this enum.
  // Currently it's not possible to switch over `SILArgumentConvention::ConventionType`,
  // because it's not a class enum.
  enum class ArgumentConvention {
    Indirect_In,
    Indirect_In_Guaranteed,
    Indirect_Inout,
    Indirect_InoutAliasable,
    Indirect_Out,
    Direct_Owned,
    Direct_Unowned,
    Direct_Guaranteed
  };

  SWIFT_IMPORT_UNSAFE
  llvm::StringRef getName() const { return getFunction()->getName(); }

  std::string getDebugDescription() const;

  bool hasOwnership() const { return getFunction()->hasOwnership(); }

  SWIFT_IMPORT_UNSAFE
  inline OptionalBridgedBasicBlock getFirstBlock() const;

  SWIFT_IMPORT_UNSAFE
  inline OptionalBridgedBasicBlock getLastBlock() const;

  SwiftInt getNumIndirectFormalResults() const {
    return (SwiftInt)getFunction()->getLoweredFunctionType()->getNumIndirectFormalResults();
  }

  SwiftInt getNumParameters() const {
    return (SwiftInt)getFunction()->getLoweredFunctionType()->getNumParameters();
  }

  SwiftInt getSelfArgumentIndex() const {
    swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
    swift::CanSILFunctionType fTy = getFunction()->getLoweredFunctionType();
    if (!fTy->hasSelfParam())
      return -1;
    return conv.getNumParameters() + conv.getNumIndirectSILResults() - 1;
  }

  SwiftInt getNumSILArguments() const {
    return swift::SILFunctionConventions(getFunction()->getConventionsInContext()).getNumSILArguments();
  }

  swift::SILType getSILArgumentType(SwiftInt idx) const {
    swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
    return conv.getSILArgumentType(idx, getFunction()->getTypeExpansionContext());
  }

  ArgumentConvention getBridged(swift::SILArgumentConvention conv) const;

  ArgumentConvention getSILArgumentConvention(SwiftInt idx) const {
    swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
    return getBridged(swift::SILArgumentConvention(conv.getParamInfoForSILArg(idx).getConvention()));
  }

  swift::SILType getSILResultType() const {
    swift::SILFunctionConventions conv(getFunction()->getConventionsInContext());
    return conv.getSILResultType(getFunction()->getTypeExpansionContext());
  }

  bool isSwift51RuntimeAvailable() const {
    if (getFunction()->getResilienceExpansion() != swift::ResilienceExpansion::Maximal)
      return false;

    swift::ASTContext &ctxt = getFunction()->getModule().getASTContext();
    return swift::AvailabilityContext::forDeploymentTarget(ctxt).isContainedIn(ctxt.getSwift51Availability());
  }

  bool isPossiblyUsedExternally() const {
    return getFunction()->isPossiblyUsedExternally();
  }

  bool isAvailableExternally() const {
    return getFunction()->isAvailableExternally();
  }

  bool hasSemanticsAttr(llvm::StringRef attrName) const {
    return getFunction()->hasSemanticsAttr(attrName) ? 1 : 0;
  }

  swift::EffectsKind getEffectAttribute() const {
    return getFunction()->getEffectsKind();
  }

  bool needsStackProtection() const {
    return getFunction()->needsStackProtection();
  }

  void setNeedStackProtection(bool needSP) const {
    getFunction()->setNeedStackProtection(needSP);
  }

  enum class ParseEffectsMode {
    argumentEffectsFromSource,
    argumentEffectsFromSIL,
    globalEffectsFromSIL,
    multipleEffectsFromSIL
  };

  struct ParsingError {
    const unsigned char * _Nullable message;
    SwiftInt position;
  };

  struct EffectInfo {
    SwiftInt argumentIndex;
    bool isDerived;
    bool isEmpty;
    bool isValid;
  };

  typedef void (* _Nonnull RegisterFn)(BridgedFunction f, void * _Nonnull data, SwiftInt size);
  typedef void (* _Nonnull WriteFn)(BridgedFunction, BridgedOStream, SwiftInt);
  typedef ParsingError (*_Nonnull ParseFn)(BridgedFunction,
                                           llvm::StringRef,
                                           ParseEffectsMode, SwiftInt,
                                           BridgedArrayRef);
  typedef SwiftInt (* _Nonnull CopyEffectsFn)(BridgedFunction, BridgedFunction);
  typedef EffectInfo (* _Nonnull GetEffectInfoFn)(BridgedFunction, SwiftInt);
  typedef BridgedMemoryBehavior (* _Nonnull GetMemBehaviorFn)(BridgedFunction, bool);

  static void registerBridging(SwiftMetatype metatype,
              RegisterFn initFn, RegisterFn destroyFn,
              WriteFn writeFn, ParseFn parseFn,
              CopyEffectsFn copyEffectsFn,
              GetEffectInfoFn effectInfoFn,
              GetMemBehaviorFn memBehaviorFn);
};

struct OptionalBridgedFunction {
  OptionalSwiftObject obj;
};

typedef struct {
  SwiftObject obj;
} BridgedGlobalVar;

struct BridgedBasicBlock {
  SwiftObject obj;
};

struct BridgedSuccessor {
  const swift::SILSuccessor * _Nonnull succ;

  SWIFT_IMPORT_UNSAFE
  inline OptionalBridgedSuccessor getNext() const;

  SWIFT_IMPORT_UNSAFE
  BridgedBasicBlock getTargetBlock() const {
    return {succ->getBB()};
  }

  SWIFT_IMPORT_UNSAFE
  inline BridgedInstruction getContainingInst() const;
};

struct OptionalBridgedSuccessor {
  const swift::SILSuccessor * _Nullable succ;

  // Assumes that `succ` is not null.
  SWIFT_IMPORT_UNSAFE
  BridgedSuccessor advancedBy(SwiftInt index) const { return {succ + index}; }
};

struct BridgedSuccessorArray {
  OptionalBridgedSuccessor base;
  SwiftInt count;
};

struct OptionalBridgedBasicBlock {
  OptionalSwiftObject obj;
};

typedef struct {
  SwiftObject obj;
} BridgedArgument;

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedArgument;

typedef struct {
  SwiftObject obj;
} BridgedNode;

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedValue;

struct BridgedInstruction {
  SwiftObject obj;
};

typedef struct {
  OptionalSwiftObject obj;
} OptionalBridgedInstruction;

typedef struct {
  SwiftObject obj;
} BridgedMultiValueResult;

struct BridgedVTableEntry {
  const swift::SILVTableEntry * _Nonnull entry;

  std::string getDebugDescription() const;

  SWIFT_IMPORT_UNSAFE
  BridgedFunction getImplementation() const {
    return {entry->getImplementation()};
  }
};

struct BridgedVTableEntryArray {
  BridgedVTableEntry base;
  SwiftInt count;
};

struct BridgedVTable {
  const swift::SILVTable * _Nonnull vTable;

  std::string getDebugDescription() const;

  SWIFT_IMPORT_UNSAFE
  BridgedVTableEntryArray getEntries() const {
    auto entries = vTable->getEntries();
    return {{entries.data()}, (SwiftInt)entries.size()};
  }
};

struct BridgedWitnessTableEntry {
  const swift::SILWitnessTable::Entry * _Nonnull entry;

  SWIFT_IMPORT_UNSAFE
  std::string getDebugDescription() const;

  swift::SILWitnessTable::WitnessKind getKind() const {
    return entry->getKind();
  }

  SWIFT_IMPORT_UNSAFE
  OptionalBridgedFunction getMethodFunction() const {
    return {entry->getMethodWitness().Witness};
  }
};

struct BridgedWitnessTableEntryArray {
  BridgedWitnessTableEntry base;
  SwiftInt count;
};

struct BridgedWitnessTable {
  const swift::SILWitnessTable * _Nonnull table;

  std::string getDebugDescription() const;

  SWIFT_IMPORT_UNSAFE
  BridgedWitnessTableEntryArray getEntries() const {
    auto entries = table->getEntries();
    return {{entries.data()}, (SwiftInt)entries.size()};
  }
};

struct OptionalBridgedWitnessTable {
  const swift::SILWitnessTable * _Nullable table;
};

struct BridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nonnull table;

  std::string getDebugDescription() const;

  SWIFT_IMPORT_UNSAFE
  BridgedWitnessTableEntryArray getEntries() const {
    auto entries = table->getEntries();
    return {{entries.data()}, (SwiftInt)entries.size()};
  }
};

struct OptionalBridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nullable table;
};

typedef struct {
  OptionalBridgedInstruction insertBefore;
  OptionalBridgedBasicBlock insertAtEnd;
  swift::SILDebugLocation loc;
} BridgedBuilder;

// AST bridging

struct BridgedNominalTypeDecl {
  swift::NominalTypeDecl * _Nonnull decl;
};

void registerBridgedClass(llvm::StringRef className, SwiftMetatype metatype);

SwiftInt PassContext_continueWithNextSubpassRun(BridgedPassContext passContext,
                                                OptionalBridgedInstruction inst);
void PassContext_notifyChanges(BridgedPassContext passContext,
                               enum ChangeNotificationKind changeKind);
BridgedBasicBlock PassContext_splitBlock(BridgedInstruction bridgedInst);
void PassContext_eraseInstruction(BridgedPassContext passContext,
                                  BridgedInstruction inst);
void PassContext_eraseBlock(BridgedPassContext passContext,
                            BridgedBasicBlock block);

llvm::StringRef SILGlobalVariable_getName(BridgedGlobalVar global);
std::string SILGlobalVariable_debugDescription(BridgedGlobalVar global);
SwiftInt SILGlobalVariable_isLet(BridgedGlobalVar global);

OptionalBridgedBasicBlock SILBasicBlock_next(BridgedBasicBlock block);
OptionalBridgedBasicBlock SILBasicBlock_previous(BridgedBasicBlock block);
BridgedFunction SILBasicBlock_getFunction(BridgedBasicBlock block);
std::string SILBasicBlock_debugDescription(BridgedBasicBlock block);
OptionalBridgedInstruction SILBasicBlock_firstInst(BridgedBasicBlock block);
OptionalBridgedInstruction SILBasicBlock_lastInst(BridgedBasicBlock block);
SwiftInt SILBasicBlock_getNumArguments(BridgedBasicBlock block);
BridgedArgument SILBasicBlock_getArgument(BridgedBasicBlock block, SwiftInt index);
BridgedArgument SILBasicBlock_addBlockArgument(BridgedBasicBlock block,
                                               swift::SILType type,
                                               BridgedValue::Ownership ownership);
void SILBasicBlock_eraseArgument(BridgedBasicBlock block, SwiftInt index);
void SILBasicBlock_moveAllInstructionsToBegin(BridgedBasicBlock block, BridgedBasicBlock dest);
void SILBasicBlock_moveAllInstructionsToEnd(BridgedBasicBlock block, BridgedBasicBlock dest);
void BasicBlock_moveArgumentsTo(BridgedBasicBlock block, BridgedBasicBlock dest);
OptionalBridgedSuccessor SILBasicBlock_getFirstPred(BridgedBasicBlock block);

std::string SILNode_debugDescription(BridgedNode node);

std::string SILLocation_debugDescription(swift::SILDebugLocation loc);
swift::SILDebugLocation
SILLocation_getAutogeneratedLocation(swift::SILDebugLocation loc);
bool SILLocation_equal(swift::SILDebugLocation lhs, swift::SILDebugLocation rhs);
bool SILLocation_hasSameSourceLocation(swift::SILDebugLocation lhs, swift::SILDebugLocation rhs);

BridgedBasicBlock SILArgument_getParent(BridgedArgument argument);
BridgedFunction::ArgumentConvention SILArgument_getConvention(BridgedArgument argument);

OptionalBridgedInstruction SILInstruction_next(BridgedInstruction inst);
OptionalBridgedInstruction SILInstruction_previous(BridgedInstruction inst);
BridgedBasicBlock SILInstruction_getParent(BridgedInstruction inst);
bool SILInstruction_isDeleted(BridgedInstruction inst);
BridgedOperandArray SILInstruction_getOperands(BridgedInstruction inst);
void SILInstruction_setOperand(BridgedInstruction inst, SwiftInt index,
                               BridgedValue value);
swift::SILDebugLocation SILInstruction_getLocation(BridgedInstruction inst);
BridgedMemoryBehavior SILInstruction_getMemBehavior(BridgedInstruction inst);
bool SILInstruction_mayRelease(BridgedInstruction inst);
bool SILInstruction_hasUnspecifiedSideEffects(BridgedInstruction inst);
bool swift_mayAccessPointer(BridgedInstruction inst);
bool swift_mayLoadWeakOrUnowned(BridgedInstruction inst);
bool swift_maySynchronizeNotConsideringSideEffects(BridgedInstruction inst);
bool swift_mayBeDeinitBarrierNotConsideringSideEffects(BridgedInstruction inst);

BridgedInstruction MultiValueInstResult_getParent(BridgedMultiValueResult result);
SwiftInt MultiValueInstResult_getIndex(BridgedMultiValueResult result);
SwiftInt MultipleValueInstruction_getNumResults(BridgedInstruction inst);
BridgedMultiValueResult
  MultipleValueInstruction_getResult(BridgedInstruction inst, SwiftInt index);

BridgedSuccessorArray TermInst_getSuccessors(BridgedInstruction term);

llvm::StringRef CondFailInst_getMessage(BridgedInstruction cfi);
SwiftInt LoadInst_getLoadOwnership(BridgedInstruction load);
swift::BuiltinValueKind BuiltinInst_getID(BridgedInstruction bi);
SwiftInt AddressToPointerInst_needsStackProtection(BridgedInstruction atp);
SwiftInt IndexAddrInst_needsStackProtection(BridgedInstruction ia);
BridgedGlobalVar GlobalAccessInst_getGlobal(BridgedInstruction globalInst);
BridgedFunction FunctionRefBaseInst_getReferencedFunction(BridgedInstruction fri);
llvm::APInt IntegerLiteralInst_getValue(BridgedInstruction ili);
llvm::StringRef StringLiteralInst_getValue(BridgedInstruction sli);
SwiftInt TupleExtractInst_fieldIndex(BridgedInstruction tei);
SwiftInt TupleElementAddrInst_fieldIndex(BridgedInstruction teai);
SwiftInt StructExtractInst_fieldIndex(BridgedInstruction sei);
OptionalBridgedValue StructInst_getUniqueNonTrivialFieldValue(BridgedInstruction si);
SwiftInt StructElementAddrInst_fieldIndex(BridgedInstruction seai);
SwiftInt ProjectBoxInst_fieldIndex(BridgedInstruction pbi);
SwiftInt EnumInst_caseIndex(BridgedInstruction ei);
SwiftInt UncheckedEnumDataInst_caseIndex(BridgedInstruction uedi);
SwiftInt InitEnumDataAddrInst_caseIndex(BridgedInstruction idea);
SwiftInt UncheckedTakeEnumDataAddrInst_caseIndex(BridgedInstruction utedi);
SwiftInt InjectEnumAddrInst_caseIndex(BridgedInstruction ieai);
SwiftInt RefElementAddrInst_fieldIndex(BridgedInstruction reai);
SwiftInt RefElementAddrInst_fieldIsLet(BridgedInstruction reai);
SwiftInt PartialApplyInst_numArguments(BridgedInstruction ai);
SwiftInt ApplyInst_numArguments(BridgedInstruction ai);
bool ApplyInst_getNonThrowing(BridgedInstruction ai);
bool ApplyInst_getNonAsync(BridgedInstruction ai);
const swift::GenericSpecializationInformation * _Nullable
ApplyInst_getSpecializationInfo(BridgedInstruction ai);
SwiftInt PartialApply_getCalleeArgIndexOfFirstAppliedArg(BridgedInstruction pai);
SwiftInt PartialApplyInst_isOnStack(BridgedInstruction pai);
SwiftInt AllocRefInstBase_isObjc(BridgedInstruction arb);
SwiftInt AllocRefInstBase_canAllocOnStack(BridgedInstruction arb);
SwiftInt BeginApplyInst_numArguments(BridgedInstruction ai);
SwiftInt TryApplyInst_numArguments(BridgedInstruction ai);
BridgedBasicBlock BranchInst_getTargetBlock(BridgedInstruction bi);
SwiftInt SwitchEnumInst_getNumCases(BridgedInstruction se);
SwiftInt SwitchEnumInst_getCaseIndex(BridgedInstruction se, SwiftInt idx);
SwiftInt StoreInst_getStoreOwnership(BridgedInstruction store);
swift::SILAccessKind BeginAccessInst_getAccessKind(BridgedInstruction beginAccess);
SwiftInt BeginAccessInst_isStatic(BridgedInstruction beginAccess);
SwiftInt CopyAddrInst_isTakeOfSrc(BridgedInstruction copyAddr);
SwiftInt CopyAddrInst_isInitializationOfDest(BridgedInstruction copyAddr);
void RefCountingInst_setIsAtomic(BridgedInstruction rc, bool isAtomic);
bool RefCountingInst_getIsAtomic(BridgedInstruction rc);
SwiftInt CondBranchInst_getNumTrueArgs(BridgedInstruction cbr);

struct KeyPathFunctionResults {
  enum { maxFunctions = 5 };
  BridgedFunction functions[maxFunctions];
  SwiftInt numFunctions;
};
SwiftInt KeyPathInst_getNumComponents(BridgedInstruction kpi);
void KeyPathInst_getReferencedFunctions(BridgedInstruction kpi, SwiftInt componentIdx,
                                            KeyPathFunctionResults * _Nonnull results);

swift::SubstitutionMap ApplySite_getSubstitutionMap(BridgedInstruction inst);
BridgedFunction::ArgumentConvention ApplySite_getArgumentConvention(BridgedInstruction inst, SwiftInt calleeArgIdx);
SwiftInt ApplySite_getNumArguments(BridgedInstruction inst);
SwiftInt FullApplySite_numIndirectResultArguments(BridgedInstruction inst);

BridgedInstruction SILBuilder_createBuiltinBinaryFunction(
          BridgedBuilder builder, llvm::StringRef name,
          swift::SILType operandType, swift::SILType resultType,
          BridgedValueArray arguments);
BridgedInstruction SILBuilder_createCondFail(BridgedBuilder builder,
          BridgedValue condition, llvm::StringRef message);
BridgedInstruction SILBuilder_createIntegerLiteral(BridgedBuilder builder,
          swift::SILType type, SwiftInt value);
BridgedInstruction SILBuilder_createAllocStack(BridgedBuilder builder,
          swift::SILType type, SwiftInt hasDynamicLifetime, SwiftInt isLexical,
          SwiftInt wasMoved);
BridgedInstruction SILBuilder_createDeallocStack(BridgedBuilder builder,
          BridgedValue operand);
BridgedInstruction SILBuilder_createDeallocStackRef(BridgedBuilder builder,
          BridgedValue operand);
BridgedInstruction SILBuilder_createUncheckedRefCast(BridgedBuilder builder,
          BridgedValue op, swift::SILType type);
BridgedInstruction SILBuilder_createSetDeallocating(BridgedBuilder builder,
          BridgedValue op, bool isAtomic);
BridgedInstruction SILBuilder_createFunctionRef(BridgedBuilder builder,
          BridgedFunction function);
BridgedInstruction SILBuilder_createCopyValue(BridgedBuilder builder,
          BridgedValue op);
BridgedInstruction SILBuilder_createCopyAddr(BridgedBuilder builder,
          BridgedValue from, BridgedValue to,
          SwiftInt takeSource, SwiftInt initializeDest);
BridgedInstruction SILBuilder_createDestroyValue(BridgedBuilder builder,
          BridgedValue op);
BridgedInstruction SILBuilder_createDebugStep(BridgedBuilder builder);
BridgedInstruction SILBuilder_createApply(BridgedBuilder builder,
          BridgedValue function, swift::SubstitutionMap subMap,
          BridgedValueArray arguments, bool isNonThrowing, bool isNonAsync,
          const swift::GenericSpecializationInformation * _Nullable specInfo);
BridgedInstruction SILBuilder_createSwitchEnumInst(BridgedBuilder builder,
          BridgedValue enumVal, OptionalBridgedBasicBlock defaultBlock,
          const void * _Nullable enumCases, SwiftInt numEnumCases);
BridgedInstruction SILBuilder_createUncheckedEnumData(BridgedBuilder builder,
          BridgedValue enumVal, SwiftInt caseIdx,
          swift::SILType resultType);
BridgedInstruction SILBuilder_createBranch(
          BridgedBuilder builder, BridgedBasicBlock destBlock,
          BridgedValueArray arguments);
BridgedInstruction SILBuilder_createUnreachable(BridgedBuilder builder);

OptionalBridgedOperand BridgedOperand::getNextUse() const {
  return {op->getNextUse()};
}

BridgedInstruction BridgedOperand::getUser() const {
  return {op->getUser()->asSILNode()};
}

OptionalBridgedOperand BridgedValue::getFirstUse() const {
  return {*getSILValue()->use_begin()};
}

OptionalBridgedSuccessor BridgedSuccessor::getNext() const {
  return {succ->getNext()};
}

BridgedInstruction BridgedSuccessor::getContainingInst() const {
  return {succ->getContainingInst()};
}

OptionalBridgedBasicBlock BridgedFunction::getFirstBlock() const {
  return {getFunction()->empty() ? nullptr : getFunction()->getEntryBlock()};
}

OptionalBridgedBasicBlock BridgedFunction::getLastBlock() const {
  return {getFunction()->empty() ? nullptr : &*getFunction()->rbegin()};
}


SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
