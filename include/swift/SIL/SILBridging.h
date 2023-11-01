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

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into SILBridgingImpl.h or SILBridging.cpp and
// required header files should be added there.
//
#include "swift/AST/ASTBridging.h"

#ifdef USED_IN_CPP_SOURCE
#include "llvm/ADT/ArrayRef.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugVariable.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILWitnessTable.h"
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct BridgedInstruction;
struct OptionalBridgedInstruction;
struct OptionalBridgedOperand;
struct OptionalBridgedSuccessor;
struct BridgedFunction;
struct BridgedBasicBlock;
struct BridgedSuccessorArray;
struct OptionalBridgedBasicBlock;

namespace swift {
class ValueBase;
class Operand;
class SILFunction;
class SILBasicBlock;
class SILSuccessor;
class SILGlobalVariable;
class SILInstruction;
class SILArgument;
class MultipleValueInstructionResult;
class SILVTableEntry;
class SILVTable;
class SILWitnessTable;
class SILDefaultWitnessTable;
class NominalTypeDecl;
class VarDecl;
class SwiftPassInvocation;
class GenericSpecializationInformation;
}

void registerBridgedClass(BridgedStringRef className, SwiftMetatype metatype);

struct BridgedType {
  void * _Nullable opaqueValue;

  enum class MetatypeRepresentation {
    Thin,
    Thick,
    ObjC
  };

  enum class TraitResult {
    IsNot,
    CanBe,
    Is
  };

#ifdef USED_IN_CPP_SOURCE
  BridgedType(swift::SILType t) : opaqueValue(t.getOpaqueValue()) {}

  swift::SILType unbridged() const {
    return swift::SILType::getFromOpaqueValue(opaqueValue);
  }
#endif

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE bool isNull() const;
  BRIDGED_INLINE bool isAddress() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getAddressType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getObjectType() const;
  BRIDGED_INLINE bool isTrivial(BridgedFunction f) const;
  BRIDGED_INLINE bool isNonTrivialOrContainsRawPointer(BridgedFunction f) const;
  BRIDGED_INLINE bool isValueTypeWithDeinit() const;
  BRIDGED_INLINE bool isLoadable(BridgedFunction f) const;
  BRIDGED_INLINE bool isReferenceCounted(BridgedFunction f) const;
  BRIDGED_INLINE bool isUnownedStorageType() const;
  BRIDGED_INLINE bool hasArchetype() const;
  BRIDGED_INLINE bool isNominalOrBoundGenericNominal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNominalTypeDecl getNominalOrBoundGenericNominal() const;
  BRIDGED_INLINE bool isClassOrBoundGenericClass() const;
  BRIDGED_INLINE bool isStructOrBoundGenericStruct() const;
  BRIDGED_INLINE bool isTuple() const;
  BRIDGED_INLINE bool isEnumOrBoundGenericEnum() const;
  BRIDGED_INLINE bool isFunction() const;
  BRIDGED_INLINE bool isMetatype() const;
  BRIDGED_INLINE bool isNoEscapeFunction() const;
  BRIDGED_INLINE bool isAsyncFunction() const;
  BRIDGED_INLINE TraitResult canBeClass() const;
  BRIDGED_INLINE bool isMoveOnly() const;
  BRIDGED_INLINE bool isEscapable() const;
  BRIDGED_INLINE bool isOrContainsObjectiveCClass() const;
  BRIDGED_INLINE bool isBuiltinInteger() const;
  BRIDGED_INLINE bool isBuiltinFloat() const;
  BRIDGED_INLINE bool isBuiltinVector() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getBuiltinVectorElementType() const;
  BRIDGED_INLINE bool isBuiltinFixedWidthInteger(SwiftInt width) const;
  BRIDGED_INLINE bool isExactSuperclassOf(BridgedType t) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getInstanceTypeOfMetatype(BridgedFunction f) const;
  BRIDGED_INLINE MetatypeRepresentation getRepresentationOfMetatype(BridgedFunction f) const;
  BRIDGED_INLINE bool isCalleeConsumedFunction() const;
  BRIDGED_INLINE bool isMarkedAsImmortal() const;
  BRIDGED_INLINE SwiftInt getCaseIdxOfEnumType(BridgedStringRef name) const;
  BRIDGED_INLINE SwiftInt getNumNominalFields() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getFieldType(SwiftInt idx, BridgedFunction f) const;
  BRIDGED_INLINE SwiftInt getFieldIdxOfNominalType(BridgedStringRef name) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getFieldName(SwiftInt idx) const;
  BRIDGED_INLINE SwiftInt getNumTupleElements() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getTupleElementType(SwiftInt idx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getFunctionTypeWithNoEscape(bool withNoEscape) const;
};

// SIL Bridging

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

#ifdef USED_IN_CPP_SOURCE
  static swift::ValueOwnershipKind castToOwnership(BridgedValue::Ownership ownership) {
    switch (ownership) {
      case BridgedValue::Ownership::Unowned:    return swift::OwnershipKind::Unowned;
      case BridgedValue::Ownership::Owned:      return swift::OwnershipKind::Owned;
      case BridgedValue::Ownership::Guaranteed: return swift::OwnershipKind::Guaranteed;
      case BridgedValue::Ownership::None:       return swift::OwnershipKind::None;
    }
  }
#endif

  Kind getKind() const;
  BRIDGED_INLINE swift::ValueBase * _Nonnull getSILValue() const;
  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand getFirstUse() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getType() const;
  BRIDGED_INLINE Ownership getOwnership() const;
};

struct OptionalBridgedValue {
  OptionalSwiftObject obj;

  BRIDGED_INLINE swift::ValueBase * _Nullable getSILValue() const;
};

// This is the layout of a class existential.
struct BridgeValueExistential {
  BridgedValue value;
  void * _Nonnull conformance;
};

struct BridgedValueArray {
  const BridgeValueExistential * _Nullable base;
  size_t count;

#ifdef USED_IN_CPP_SOURCE
  llvm::ArrayRef<swift::SILValue> getValues(llvm::SmallVectorImpl<swift::SILValue> &storage);
#endif
};

struct BridgedOperand {
  swift::Operand * _Nonnull op;

  enum class OperandOwnership {
    NonUse,
    TrivialUse,
    InstantaneousUse,
    UnownedInstantaneousUse,
    ForwardingUnowned,
    PointerEscape,
    BitwiseEscape,
    Borrow,
    DestroyingConsume,
    ForwardingConsume,
    InteriorPointer,
    GuaranteedForwarding,
    EndBorrow,
    Reborrow
  };

  BRIDGED_INLINE bool isTypeDependent() const;
  BRIDGED_INLINE bool isLifetimeEnding() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand getNextUse() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue getValue() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getUser() const;
  BRIDGED_INLINE OperandOwnership getOperandOwnership() const;
};

struct OptionalBridgedOperand {
  swift::Operand * _Nullable op;

  // Assumes that `op` is not null.
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedOperand advancedBy(SwiftInt index) const;

  // Assumes that `op` is not null.
  BRIDGED_INLINE SwiftInt distanceTo(BridgedOperand element) const;
};

struct BridgedOperandArray {
  OptionalBridgedOperand base;
  SwiftInt count;
};

// Unfortunately we need to take a detour over this enum.
// Currently it's not possible to switch over `SILArgumentConvention::ConventionType`,
// because it's not a class enum.
enum class BridgedArgumentConvention {
  Indirect_In,
  Indirect_In_Guaranteed,
  Indirect_Inout,
  Indirect_InoutAliasable,
  Indirect_Out,
  Direct_Owned,
  Direct_Unowned,
  Direct_Guaranteed,
  Pack_Owned,
  Pack_Inout,
  Pack_Guaranteed,
  Pack_Out
};

enum class BridgedMemoryBehavior {
  None,
  MayRead,
  MayWrite,
  MayReadWrite,
  MayHaveSideEffects
};

struct BridgedFunction {
  SwiftObject obj;

  enum class EffectsKind {
    ReadNone,
    ReadOnly,
    ReleaseNone,
    ReadWrite,
    Unspecified,
    Custom
  };

  enum class PerformanceConstraints {
    None = 0,
    NoAllocation = 1,
    NoLocks = 2
  };

  enum class InlineStrategy {
    InlineDefault,
    NoInline,
    AlwaysInline
  };

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE swift::SILFunction * _Nonnull getFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getName() const;
  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE bool hasOwnership() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getFirstBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getLastBlock() const;
  BRIDGED_INLINE SwiftInt getNumIndirectFormalResults() const;
  BRIDGED_INLINE SwiftInt getNumParameters() const;
  BRIDGED_INLINE SwiftInt getSelfArgumentIndex() const;
  BRIDGED_INLINE SwiftInt getNumSILArguments() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getSILArgumentType(SwiftInt idx) const;
  BRIDGED_INLINE BridgedArgumentConvention getSILArgumentConvention(SwiftInt idx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getSILResultType() const;
  BRIDGED_INLINE bool isSwift51RuntimeAvailable() const;
  BRIDGED_INLINE bool isPossiblyUsedExternally() const;
  BRIDGED_INLINE bool isAvailableExternally() const;
  BRIDGED_INLINE bool isTransparent() const;
  BRIDGED_INLINE bool isAsync() const;
  BRIDGED_INLINE bool isGlobalInitFunction() const;
  BRIDGED_INLINE bool isGlobalInitOnceFunction() const;
  BRIDGED_INLINE bool isDestructor() const;
  BRIDGED_INLINE bool isGenericFunction() const;
  BRIDGED_INLINE bool hasSemanticsAttr(BridgedStringRef attrName) const;
  BRIDGED_INLINE bool hasUnsafeNonEscapableResult() const;
  BRIDGED_INLINE EffectsKind getEffectAttribute() const;
  BRIDGED_INLINE PerformanceConstraints getPerformanceConstraints() const;
  BRIDGED_INLINE InlineStrategy getInlineStrategy() const;
  BRIDGED_INLINE bool isSerialized() const;
  BRIDGED_INLINE bool hasValidLinkageForFragileRef() const;
  BRIDGED_INLINE bool needsStackProtection() const;
  BRIDGED_INLINE void setNeedStackProtection(bool needSP) const;

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
                                           BridgedStringRef,
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

struct BridgedGlobalVar {
  SwiftObject obj;

  BridgedGlobalVar(SwiftObject obj) : obj(obj) {}
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE swift::SILGlobalVariable * _Nonnull getGlobal() const;
  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getName() const;
  BRIDGED_INLINE bool isLet() const;
  BRIDGED_INLINE void setLet(bool value) const;
  BRIDGED_INLINE bool isPossiblyUsedExternally() const;
  BRIDGED_INLINE bool isAvailableExternally() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getFirstStaticInitInst() const;
  bool canBeInitializedStatically() const;
  bool mustBeInitializedStatically() const;
};

struct OptionalBridgedGlobalVar {
  OptionalSwiftObject obj;
};

struct BridgedMultiValueResult {
  SwiftObject obj;

#ifdef USED_IN_CPP_SOURCE
  swift::MultipleValueInstructionResult * _Nonnull unbridged() const {
    return static_cast<swift::MultipleValueInstructionResult *>(obj);
  }
#endif

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getParent() const;
  BRIDGED_INLINE SwiftInt getIndex() const;
};

struct BridgedSubstitutionMap {
  uint64_t storage[1];

#ifdef USED_IN_CPP_SOURCE
  BridgedSubstitutionMap(swift::SubstitutionMap map) {
    *reinterpret_cast<swift::SubstitutionMap *>(&storage) = map;
  }
  swift::SubstitutionMap unbridged() const {
    return *reinterpret_cast<const swift::SubstitutionMap *>(&storage);
  }
#endif

  BRIDGED_INLINE BridgedSubstitutionMap();
  BRIDGED_INLINE bool isEmpty() const;
};

struct BridgedTypeArray {
  BridgedArrayRef typeArray;

#ifdef USED_IN_CPP_SOURCE
  BridgedTypeArray(llvm::ArrayRef<swift::Type> types) : typeArray(types) {}

  llvm::ArrayRef<swift::Type> unbridged() const {
    return typeArray.unbridged<swift::Type>();
  }
#endif

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedTypeArray fromReplacementTypes(BridgedSubstitutionMap substMap);

  SwiftInt getCount() const { return SwiftInt(typeArray.Length); }

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedType getAt(SwiftInt index) const;
};

struct BridgedSILTypeArray {
  BridgedArrayRef typeArray;

#ifdef USED_IN_CPP_SOURCE
  BridgedSILTypeArray(llvm::ArrayRef<swift::SILType> silTypes)
      : typeArray(silTypes) {}

  llvm::ArrayRef<swift::SILType> unbridged() const {
    return typeArray.unbridged<swift::SILType>();
  }
#endif

  SwiftInt getCount() const { return SwiftInt(typeArray.Length); }

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedType getAt(SwiftInt index) const;
};

struct BridgedLocation {
  uint64_t storage[3];

#ifdef USED_IN_CPP_SOURCE
  BridgedLocation(const swift::SILDebugLocation &loc) {
    *reinterpret_cast<swift::SILDebugLocation *>(&storage) = loc;
  }
  const swift::SILDebugLocation &getLoc() const {
    return *reinterpret_cast<const swift::SILDebugLocation *>(&storage);
  }
#endif

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getAutogeneratedLocation() const;
  BRIDGED_INLINE bool hasValidLineNumber() const;
  BRIDGED_INLINE bool isAutoGenerated() const;
  BRIDGED_INLINE bool isEqualTo(BridgedLocation rhs) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSourceLoc getSourceLocation() const;
  BRIDGED_INLINE bool hasSameSourceLocation(BridgedLocation rhs) const;
  static BRIDGED_INLINE BridgedLocation getArtificialUnreachableLocation();
};

struct BridgedGenericSpecializationInformation {
  const swift::GenericSpecializationInformation * _Nullable data = nullptr;
};

struct OptionalBridgedSILDebugVariable {
  uint64_t storage[16];

#ifdef USED_IN_CPP_SOURCE
  using OptionalSILDebugVariable = llvm::Optional<swift::SILDebugVariable>;

  OptionalBridgedSILDebugVariable(
    OptionalSILDebugVariable &&debugVariable) {
    static_assert(sizeof(OptionalSILDebugVariable) <= 16*8);
    *reinterpret_cast<OptionalSILDebugVariable *>(&storage) = debugVariable;
  }
  const OptionalSILDebugVariable &getDebugVar() const {
    return *reinterpret_cast<const OptionalSILDebugVariable *>(&storage);
  }
#endif
};

struct BridgedInstruction {
  SwiftObject obj;

#ifdef USED_IN_CPP_SOURCE
  template <class I> I *_Nonnull getAs() const {
    return llvm::cast<I>(static_cast<swift::SILNode *>(obj)->castToInstruction());
  }
  swift::SILInstruction * _Nonnull unbridged() const {
    return getAs<swift::SILInstruction>();
  }
#endif

  BridgedInstruction(SwiftObject obj) : obj(obj) {}
  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getNext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getPrevious() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock getParent() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getLastInstOfParent() const;
  BRIDGED_INLINE bool isDeleted() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOperandArray getOperands() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOperandArray getTypeDependentOperands() const;
  BRIDGED_INLINE void setOperand(SwiftInt index, BridgedValue value) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getLocation() const;
  BRIDGED_INLINE BridgedMemoryBehavior getMemBehavior() const;
  BRIDGED_INLINE bool mayRelease() const;
  BRIDGED_INLINE bool mayHaveSideEffects() const;
  BRIDGED_INLINE bool maySuspend() const;
  bool mayAccessPointer() const;
  bool mayLoadWeakOrUnowned() const;
  bool maySynchronizeNotConsideringSideEffects() const;
  bool mayBeDeinitBarrierNotConsideringSideEffects() const;

  // =========================================================================//
  //                   Generalized instruction subclasses
  // =========================================================================//
  
  BRIDGED_INLINE SwiftInt MultipleValueInstruction_getNumResults() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedMultiValueResult MultipleValueInstruction_getResult(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSuccessorArray TermInst_getSuccessors() const;

  // =========================================================================//
  //                         Instruction protocols
  // =========================================================================//

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand ForwardingInst_singleForwardedOperand() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOperandArray ForwardingInst_forwardedOperands() const;
  BRIDGED_INLINE BridgedValue::Ownership ForwardingInst_forwardingOwnership() const;
  BRIDGED_INLINE bool ForwardingInst_preservesOwnership() const;

  // =========================================================================//
  //                    Specific instruction subclasses
  // =========================================================================//

  enum class BuiltinValueKind {
    None = 0,
#define BUILTIN(Id, Name, Attrs) Id,
#include "swift/AST/Builtins.def"
  };

  enum class IntrinsicID {
    memcpy, memmove,
    unknown
  };

  struct OptionalInt {
    SwiftInt value;
    bool hasValue;
  };

  enum class AccessKind {
    Init,
    Read,
    Modify,
    Deinit
  };

  struct KeyPathFunctionResults {
    enum { maxFunctions = 5 };
    BridgedFunction functions[maxFunctions];
    SwiftInt numFunctions;
  };

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef CondFailInst_getMessage() const;
  BRIDGED_INLINE SwiftInt LoadInst_getLoadOwnership() const ;
  BRIDGED_INLINE BuiltinValueKind BuiltinInst_getID() const;
  BRIDGED_INLINE IntrinsicID BuiltinInst_getIntrinsicID() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap BuiltinInst_getSubstitutionMap() const;
  BRIDGED_INLINE bool PointerToAddressInst_isStrict() const;
  BRIDGED_INLINE bool AddressToPointerInst_needsStackProtection() const;
  BRIDGED_INLINE bool IndexAddrInst_needsStackProtection() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGlobalVar GlobalAccessInst_getGlobal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGlobalVar AllocGlobalInst_getGlobal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction FunctionRefBaseInst_getReferencedFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalInt IntegerLiteralInst_getValue() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef StringLiteralInst_getValue() const;
  BRIDGED_INLINE int StringLiteralInst_getEncoding() const;
  BRIDGED_INLINE SwiftInt TupleExtractInst_fieldIndex() const;
  BRIDGED_INLINE SwiftInt TupleElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE SwiftInt StructExtractInst_fieldIndex() const;
  BRIDGED_INLINE OptionalBridgedValue StructInst_getUniqueNonTrivialFieldValue() const;
  BRIDGED_INLINE SwiftInt StructElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE SwiftInt ProjectBoxInst_fieldIndex() const;
  BRIDGED_INLINE bool EndCOWMutationInst_doKeepUnique() const;
  BRIDGED_INLINE SwiftInt EnumInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt UncheckedEnumDataInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt InitEnumDataAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt UncheckedTakeEnumDataAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt InjectEnumAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt RefElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE bool RefElementAddrInst_fieldIsLet() const;
  BRIDGED_INLINE bool RefElementAddrInst_isImmutable() const;
  BRIDGED_INLINE void RefElementAddrInst_setImmutable(bool isImmutable) const;
  BRIDGED_INLINE SwiftInt PartialApplyInst_numArguments() const;
  BRIDGED_INLINE SwiftInt ApplyInst_numArguments() const;
  BRIDGED_INLINE bool ApplyInst_getNonThrowing() const;
  BRIDGED_INLINE bool ApplyInst_getNonAsync() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGenericSpecializationInformation ApplyInst_getSpecializationInfo() const;
  BRIDGED_INLINE SwiftInt ObjectInst_getNumBaseElements() const;
  BRIDGED_INLINE SwiftInt PartialApply_getCalleeArgIndexOfFirstAppliedArg() const;
  BRIDGED_INLINE bool PartialApplyInst_isOnStack() const;
  BRIDGED_INLINE bool AllocStackInst_hasDynamicLifetime() const;
  BRIDGED_INLINE bool AllocRefInstBase_isObjc() const;
  BRIDGED_INLINE bool AllocRefInstBase_canAllocOnStack() const;
  BRIDGED_INLINE SwiftInt AllocRefInstBase_getNumTailTypes() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSILTypeArray AllocRefInstBase_getTailAllocatedTypes() const;
  BRIDGED_INLINE bool AllocRefDynamicInst_isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType() const;
  BRIDGED_INLINE SwiftInt BeginApplyInst_numArguments() const;
  BRIDGED_INLINE SwiftInt TryApplyInst_numArguments() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock BranchInst_getTargetBlock() const;
  BRIDGED_INLINE SwiftInt SwitchEnumInst_getNumCases() const;
  BRIDGED_INLINE SwiftInt SwitchEnumInst_getCaseIndex(SwiftInt idx) const;
  BRIDGED_INLINE SwiftInt StoreInst_getStoreOwnership() const;
  BRIDGED_INLINE SwiftInt AssignInst_getAssignOwnership() const;
  BRIDGED_INLINE AccessKind BeginAccessInst_getAccessKind() const;
  BRIDGED_INLINE bool BeginAccessInst_isStatic() const;
  BRIDGED_INLINE bool CopyAddrInst_isTakeOfSrc() const;
  BRIDGED_INLINE bool CopyAddrInst_isInitializationOfDest() const;
  BRIDGED_INLINE SwiftInt MarkUninitializedInst_getKind() const;
  BRIDGED_INLINE void RefCountingInst_setIsAtomic(bool isAtomic) const;
  BRIDGED_INLINE bool RefCountingInst_getIsAtomic() const;
  BRIDGED_INLINE SwiftInt CondBranchInst_getNumTrueArgs() const;
  BRIDGED_INLINE void AllocRefInstBase_setIsStackAllocatable() const;
  BRIDGED_INLINE bool AllocRefInst_isBare() const;
  BRIDGED_INLINE void AllocRefInst_setIsBare() const;
  BRIDGED_INLINE void TermInst_replaceBranchTarget(BridgedBasicBlock from, BridgedBasicBlock to) const;
  BRIDGED_INLINE SwiftInt KeyPathInst_getNumComponents() const;
  BRIDGED_INLINE void KeyPathInst_getReferencedFunctions(SwiftInt componentIdx, KeyPathFunctionResults * _Nonnull results) const;
  BRIDGED_INLINE bool GlobalValueInst_isBare() const;
  BRIDGED_INLINE void GlobalValueInst_setIsBare() const;
  BRIDGED_INLINE void LoadInst_setOwnership(SwiftInt ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastBranch_getSuccessBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastBranch_getFailureBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap ApplySite_getSubstitutionMap() const;
  BRIDGED_INLINE BridgedArgumentConvention ApplySite_getArgumentConvention(SwiftInt calleeArgIdx) const;
  BRIDGED_INLINE SwiftInt ApplySite_getNumArguments() const;
  BRIDGED_INLINE SwiftInt FullApplySite_numIndirectResultArguments() const;

  // =========================================================================//
  //                   VarDeclInst and DebugVariableInst
  // =========================================================================//

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNullableVarDecl
  DebugValue_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNullableVarDecl
  AllocStack_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNullableVarDecl
  AllocBox_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNullableVarDecl
  GlobalAddr_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedNullableVarDecl
  RefElementAddr_getDecl() const;

  BRIDGED_INLINE OptionalBridgedSILDebugVariable DebugValue_getVarInfo() const;

  BRIDGED_INLINE OptionalBridgedSILDebugVariable AllocStack_getVarInfo() const;

  BRIDGED_INLINE OptionalBridgedSILDebugVariable AllocBox_getVarInfo() const;
};

struct OptionalBridgedInstruction {
  OptionalSwiftObject obj;

#ifdef USED_IN_CPP_SOURCE
  swift::SILInstruction * _Nullable unbridged() const {
    if (!obj)
      return nullptr;
    return llvm::cast<swift::SILInstruction>(static_cast<swift::SILNode *>(obj)->castToInstruction());
  }
#endif

  OptionalBridgedInstruction() : obj(nullptr) {}
  OptionalBridgedInstruction(OptionalSwiftObject obj) : obj(obj) {}
};

struct BridgedArgument {
  SwiftObject obj;

  BRIDGED_INLINE swift::SILArgument * _Nonnull getArgument() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock getParent() const;
  BRIDGED_INLINE BridgedArgumentConvention getConvention() const;
  BRIDGED_INLINE bool isSelf() const;
};

struct OptionalBridgedArgument {
  OptionalSwiftObject obj;
};

struct OptionalBridgedBasicBlock {
  OptionalSwiftObject obj;

#ifdef USED_IN_CPP_SOURCE
  swift::SILBasicBlock * _Nullable unbridged() const {
    return obj ? static_cast<swift::SILBasicBlock *>(obj) : nullptr;
  }
#endif
};

struct BridgedBasicBlock {
  SwiftObject obj;

  BridgedBasicBlock(SwiftObject obj) : obj(obj) {
  }

#ifdef USED_IN_CPP_SOURCE
  BridgedBasicBlock(swift::SILBasicBlock * _Nonnull block) : obj(block) {
  }
  swift::SILBasicBlock * _Nonnull unbridged() const {
    return static_cast<swift::SILBasicBlock *>(obj);
  }
#endif

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getNext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getPrevious() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getFirstInst() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getLastInst() const;
  BRIDGED_INLINE SwiftInt getNumArguments() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedArgument getArgument(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE  BRIDGED_INLINE BridgedArgument addBlockArgument(BridgedType type, BridgedValue::Ownership ownership) const;
  BRIDGED_INLINE void eraseArgument(SwiftInt index) const;
  BRIDGED_INLINE void moveAllInstructionsToBegin(BridgedBasicBlock dest) const;
  BRIDGED_INLINE void moveAllInstructionsToEnd(BridgedBasicBlock dest) const;
  BRIDGED_INLINE void moveArgumentsTo(BridgedBasicBlock dest) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedSuccessor getFirstPred() const;
};

struct BridgedSuccessor {
  const swift::SILSuccessor * _Nonnull succ;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedSuccessor getNext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock getTargetBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE  BridgedInstruction getContainingInst() const;
};

struct OptionalBridgedSuccessor {
  const swift::SILSuccessor * _Nullable succ;

  // Assumes that `succ` is not null.
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSuccessor advancedBy(SwiftInt index) const;
};

struct BridgedSuccessorArray {
  OptionalBridgedSuccessor base;
  SwiftInt count;
};

struct BridgedVTableEntry {
  const swift::SILVTableEntry * _Nonnull entry;

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getImplementation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedVTableEntry advanceBy(SwiftInt index) const;
};

struct BridgedVTableEntryArray {
  BridgedVTableEntry base;
  SwiftInt count;
};

struct BridgedVTable {
  const swift::SILVTable * _Nonnull vTable;

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedVTableEntryArray getEntries() const;
};

struct BridgedWitnessTableEntry {
  const void * _Nonnull entry;

  enum class Kind {
    Invalid,
    Method,
    AssociatedType,
    AssociatedTypeProtocol,
    BaseProtocol
  };

#ifdef USED_IN_CPP_SOURCE
  const swift::SILWitnessTable::Entry * _Nonnull getEntry() const {
    return (const swift::SILWitnessTable::Entry * _Nonnull)entry;
  }
#endif

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE Kind getKind() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedFunction getMethodFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTableEntry advanceBy(SwiftInt index) const;
};

struct BridgedWitnessTableEntryArray {
  BridgedWitnessTableEntry base;
  SwiftInt count;
};

struct BridgedWitnessTable {
  const swift::SILWitnessTable * _Nonnull table;

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTableEntryArray getEntries() const;
};

struct OptionalBridgedWitnessTable {
  const swift::SILWitnessTable * _Nullable table;
};

struct BridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nonnull table;

  SWIFT_IMPORT_UNSAFE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTableEntryArray getEntries() const;
};

struct OptionalBridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nullable table;
};

struct BridgedBuilder{

  enum class InsertAt {
    beforeInst, endOfBlock, intoGlobal
  } insertAt;

  SwiftObject insertionObj;
  BridgedLocation loc;

#ifdef USED_IN_CPP_SOURCE
  swift::SILBuilder unbridged() const {
    switch (insertAt) {
    case BridgedBuilder::InsertAt::beforeInst:
      return swift::SILBuilder(BridgedInstruction(insertionObj).unbridged(),
                               loc.getLoc().getScope());
    case BridgedBuilder::InsertAt::endOfBlock:
      return swift::SILBuilder(BridgedBasicBlock(insertionObj).unbridged(),
                               loc.getLoc().getScope());
    case BridgedBuilder::InsertAt::intoGlobal:
      return swift::SILBuilder(BridgedGlobalVar(insertionObj).getGlobal());
    }
  }
  swift::SILLocation regularLoc() const {
    return swift::RegularLocation(loc.getLoc().getLocation());
  }
#endif

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBuiltinBinaryFunction(BridgedStringRef name,
                                          BridgedType operandType, BridgedType resultType,
                                          BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCondFail(BridgedValue condition,
                                                                       BridgedStringRef message) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createIntegerLiteral(BridgedType type, SwiftInt value) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createAllocStack(BridgedType type,
                                          bool hasDynamicLifetime, bool isLexical, bool wasMoved) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDeallocStack(BridgedValue operand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDeallocStackRef(BridgedValue operand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedRefCast(BridgedValue op,
                                                                               BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUpcast(BridgedValue op, BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createLoad(BridgedValue op, SwiftInt ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBeginDeallocRef(BridgedValue reference,
                                                                              BridgedValue allocation) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndInitLetRef(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStrongRetain(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStrongRelease(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnownedRetain(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnownedRelease(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createFunctionRef(BridgedFunction function) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCopyValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBeginBorrow(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndBorrow(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCopyAddr(BridgedValue from, BridgedValue to,
                                          bool takeSource, bool initializeDest) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestroyValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestroyAddr(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDebugStep() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createApply(BridgedValue function,
                                          BridgedSubstitutionMap subMap,
                                          BridgedValueArray arguments, bool isNonThrowing, bool isNonAsync,
                                          BridgedGenericSpecializationInformation specInfo) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createSwitchEnumInst(BridgedValue enumVal,
                                          OptionalBridgedBasicBlock defaultBlock,
                                          const void * _Nullable enumCases, SwiftInt numEnumCases) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedEnumData(BridgedValue enumVal,
                                          SwiftInt caseIdx, BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEnum(SwiftInt caseIdx, OptionalBridgedValue payload,
                                          BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createThinToThickFunction(BridgedValue fn,
                                                                                  BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBranch(BridgedBasicBlock destBlock,
                                                                     BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnreachable() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createObject(BridgedType type, BridgedValueArray arguments,
                                                                     SwiftInt numBaseElements) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createGlobalAddr(BridgedGlobalVar global) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createGlobalValue(BridgedGlobalVar global,
                                                                          bool isBare) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStruct(BridgedType type,
                                                                     BridgedValueArray elements) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStructExtract(BridgedValue str,
                                                                            SwiftInt fieldIndex) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStructElementAddr(BridgedValue addr,
                                                                                SwiftInt fieldIndex) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestructureStruct(BridgedValue str) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createTuple(BridgedType type,
                                                                    BridgedValueArray elements) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createTupleExtract(BridgedValue str,
                                                                           SwiftInt elementIndex) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createTupleElementAddr(BridgedValue addr,
                                                                               SwiftInt elementIndex) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestructureTuple(BridgedValue str) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStore(BridgedValue src, BridgedValue dst,
                                          SwiftInt ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createInitExistentialRef(BridgedValue instance,
                                          BridgedType type,
                                          BridgedInstruction useConformancesOf) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createMetatype(BridgedType type,
                                          BridgedType::MetatypeRepresentation representation) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndCOWMutation(BridgedValue instance,
                                                                             bool keepUnique) const;
};

// Passmanager and Context

namespace swift {
  class SwiftPassInvocation;
}

struct BridgedChangeNotificationHandler {
  swift::SwiftPassInvocation * _Nonnull invocation;

  enum class Kind {
    instructionsChanged,
    callsChanged,
    branchesChanged,
    effectsChanged
  };

  void notifyChanges(Kind changeKind) const;
};

namespace swift::test {
struct Arguments;
class FunctionTest;
} // namespace swift::test

struct BridgedFunctionTest {
  swift::test::FunctionTest *_Nonnull test;
};

struct BridgedTestArguments {
  swift::test::Arguments *_Nonnull arguments;

  bool hasUntaken() const;
  SWIFT_IMPORT_UNSAFE BridgedStringRef takeString() const;
  bool takeBool() const;
  SwiftInt takeInt() const;
  SWIFT_IMPORT_UNSAFE BridgedOperand takeOperand() const;
  SWIFT_IMPORT_UNSAFE BridgedValue takeValue() const;
  SWIFT_IMPORT_UNSAFE BridgedInstruction takeInstruction() const;
  SWIFT_IMPORT_UNSAFE BridgedArgument takeArgument() const;
  SWIFT_IMPORT_UNSAFE BridgedBasicBlock takeBlock() const;
  SWIFT_IMPORT_UNSAFE BridgedFunction takeFunction() const;
};

struct BridgedSwiftPassInvocation {
  swift::SwiftPassInvocation *_Nonnull invocation;
};

using SwiftNativeFunctionTestThunk =
    void (*_Nonnull)(void *_Nonnull, BridgedFunction, BridgedTestArguments,
                     BridgedSwiftPassInvocation);

void registerFunctionTestThunk(SwiftNativeFunctionTestThunk);

void registerFunctionTest(BridgedStringRef,
                          void *_Nonnull nativeSwiftInvocation);

void writeCharToStderr(int c);

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file. This is because they rely on C++ headers that
// we don't want to pull in when using "pure bridging mode".
#include "SILBridgingImpl.h"
#endif

#endif
