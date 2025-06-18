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
// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
//
#include "swift/AST/ASTBridging.h"

#ifdef USED_IN_CPP_SOURCE
#include "llvm/ADT/ArrayRef.h"
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
struct BridgedDeclRef;

namespace swift {
class ValueBase;
class Operand;
struct SILDebugVariable;
class ForwardingInstruction;
class SILType;
class SILFunction;
class SILBasicBlock;
class SILSuccessor;
class SILGlobalVariable;
class SILInstruction;
class SILArgument;
class MultipleValueInstructionResult;
struct ValueOwnershipKind;
class SILVTableEntry;
class SILVTable;
class ConstExprFunctionState;
class SymbolicValueBumpAllocator;
class ConstExprEvaluator;
class SILWitnessTable;
class SILDefaultWitnessTable;
class SILDebugLocation;
class NominalTypeDecl;
class VarDecl;
class SwiftPassInvocation;
class GenericSpecializationInformation;
class LifetimeDependenceInfo;
class IndexSubset;
enum class ResultConvention : uint8_t;
class SILResultInfo;
class SILParameterInfo;
struct SILDeclRef;
class SILBuilder;
class SILLocation;
}

bool swiftModulesInitialized();
void registerBridgedClass(BridgedStringRef className, SwiftMetatype metatype);

enum class BridgedResultConvention {
  Indirect,
  Owned,
  Unowned,
  UnownedInnerPointer,
  Autoreleased,
  Pack
};

struct BridgedResultInfo {
  swift::TypeBase * _Nonnull type;
  BridgedResultConvention convention;

  BRIDGED_INLINE static BridgedResultConvention castToResultConvention(swift::ResultConvention convention);
  BRIDGED_INLINE BridgedResultInfo(swift::SILResultInfo resultInfo);
};

struct OptionalBridgedResultInfo {
  swift::TypeBase * _Nullable type;
  BridgedResultConvention convention;
};

struct BridgedResultInfoArray {
  BridgedArrayRef resultInfoArray;

  BRIDGED_INLINE SwiftInt count() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedResultInfo at(SwiftInt resultIndex) const;
};

// Unfortunately we need to take a detour over this enum.
// Currently it's not possible to switch over `SILArgumentConvention::ConventionType`,
// because it's not a class enum.
enum class BridgedArgumentConvention {
  Indirect_In,
  Indirect_In_Guaranteed,
  Indirect_Inout,
  Indirect_InoutAliasable,
  Indirect_In_CXX,
  Indirect_Out,
  Direct_Owned,
  Direct_Unowned,
  Direct_Guaranteed,
  Pack_Owned,
  Pack_Inout,
  Pack_Guaranteed,
  Pack_Out
};

struct BridgedParameterInfo {
  BridgedCanType type;
  BridgedArgumentConvention convention;
  uint8_t options;

  BridgedParameterInfo(BridgedCanType type, BridgedArgumentConvention convention, uint8_t options) :
    type(type), convention(convention), options(options) {}

  BRIDGED_INLINE BridgedParameterInfo(swift::SILParameterInfo parameterInfo);
  BRIDGED_INLINE swift::SILParameterInfo unbridged() const;
};

struct BridgedParameterInfoArray {
  BridgedArrayRef parameterInfoArray;

  BRIDGED_INLINE SwiftInt count() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedParameterInfo at(SwiftInt parameterIndex) const;
};

struct BridgedYieldInfoArray {
  BridgedArrayRef yieldInfoArray;

  BRIDGED_INLINE SwiftInt count() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedParameterInfo at(SwiftInt resultIndex) const;
};
struct BridgedLifetimeDependenceInfo {
  swift::IndexSubset *_Nullable inheritLifetimeParamIndices;
  swift::IndexSubset *_Nullable scopeLifetimeParamIndices;
  swift::IndexSubset *_Nullable addressableParamIndices;
  swift::IndexSubset *_Nullable conditionallyAddressableParamIndices;
  SwiftUInt targetIndex;
  bool immortal;

  BRIDGED_INLINE BridgedLifetimeDependenceInfo(swift::LifetimeDependenceInfo info);

  BRIDGED_INLINE bool empty() const;
  BRIDGED_INLINE bool checkInherit(SwiftInt index) const;
  BRIDGED_INLINE bool checkScope(SwiftInt index) const;
  BRIDGED_INLINE bool checkAddressable(SwiftInt index) const;
  BRIDGED_INLINE bool checkConditionallyAddressable(SwiftInt index) const;
  BRIDGED_INLINE SwiftInt getTargetIndex() const;

  BRIDGED_INLINE BridgedOwnedString getDebugDescription() const;
};

struct BridgedLifetimeDependenceInfoArray {
  BridgedArrayRef lifetimeDependenceInfoArray;

  BRIDGED_INLINE SwiftInt count() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLifetimeDependenceInfo
  at(SwiftInt index) const;
};

enum class BridgedLinkage {
  Public,
  PublicNonABI,
  Package,
  PackageNonABI,
  Hidden,
  Shared,
  Private,
  PublicExternal,
  PackageExternal,
  HiddenExternal
};

// =========================================================================//
//                              SILFunctionType
// =========================================================================//

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
BridgedResultInfoArray SILFunctionType_getResultsWithError(BridgedCanType);

BRIDGED_INLINE SwiftInt
SILFunctionType_getNumIndirectFormalResultsWithError(BridgedCanType);

BRIDGED_INLINE SwiftInt SILFunctionType_getNumPackResults(BridgedCanType);

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedResultInfo SILFunctionType_getErrorResult(BridgedCanType);

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
BridgedParameterInfoArray SILFunctionType_getParameters(BridgedCanType);

BRIDGED_INLINE bool SILFunctionType_hasSelfParam(BridgedCanType);

BRIDGED_INLINE bool SILFunctionType_isTrivialNoescape(BridgedCanType);

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
BridgedYieldInfoArray SILFunctionType_getYields(BridgedCanType);

SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLifetimeDependenceInfoArray
SILFunctionType_getLifetimeDependencies(BridgedCanType);


struct BridgedType {
  void * _Nullable opaqueValue;

  struct EnumElementIterator {
    uint64_t storage[4];

    SWIFT_IMPORT_UNSAFE BRIDGED_INLINE EnumElementIterator getNext() const;
  };

  BRIDGED_INLINE BridgedType(); // for Optional<Type>.nil
  BRIDGED_INLINE BridgedType(swift::SILType t);
  BRIDGED_INLINE swift::SILType unbridged() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType getCanType() const;

  static SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType createSILType(BridgedCanType canTy);
  BRIDGED_INLINE BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE bool isNull() const;
  BRIDGED_INLINE bool isAddress() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getAddressType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getObjectType() const;
  BRIDGED_INLINE bool isTrivial(BridgedFunction f) const;
  BRIDGED_INLINE bool isNonTrivialOrContainsRawPointer(BridgedFunction f) const;
  BRIDGED_INLINE bool isLoadable(BridgedFunction f) const;
  BRIDGED_INLINE bool isReferenceCounted(BridgedFunction f) const;
  BRIDGED_INLINE bool containsNoEscapeFunction() const;
  BRIDGED_INLINE bool isEmpty(BridgedFunction f) const;
  BRIDGED_INLINE bool isMoveOnly() const;
  BRIDGED_INLINE bool isEscapable(BridgedFunction f) const;
  BRIDGED_INLINE bool isExactSuperclassOf(BridgedType t) const;
  BRIDGED_INLINE bool isMarkedAsImmortal() const;
  BRIDGED_INLINE bool isAddressableForDeps(BridgedFunction f) const;
  BRIDGED_INLINE SwiftInt getCaseIdxOfEnumType(BridgedStringRef name) const;
  static BRIDGED_INLINE SwiftInt getNumBoxFields(BridgedCanType boxTy);
  static SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getBoxFieldType(BridgedCanType boxTy,
                                                                        SwiftInt idx, BridgedFunction f);
  static BRIDGED_INLINE bool getBoxFieldIsMutable(BridgedCanType boxTy, SwiftInt idx);
  BRIDGED_INLINE SwiftInt getNumNominalFields() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getFieldType(SwiftInt idx, BridgedFunction f) const;
  BRIDGED_INLINE SwiftInt getFieldIdxOfNominalType(BridgedStringRef name) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getFieldName(SwiftInt idx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE EnumElementIterator getFirstEnumCaseIterator() const;
  BRIDGED_INLINE bool isEndCaseIterator(EnumElementIterator i) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getEnumCasePayload(EnumElementIterator i, BridgedFunction f) const;
  BRIDGED_INLINE SwiftInt getNumTupleElements() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType
  getTupleElementType(SwiftInt idx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getFunctionTypeWithNoEscape(bool withNoEscape) const;
  BRIDGED_INLINE BridgedArgumentConvention getCalleeConvention() const;
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

  BRIDGED_INLINE static swift::ValueOwnershipKind unbridge(Ownership ownership);

  Kind getKind() const;
  BRIDGED_INLINE swift::ValueBase * _Nonnull getSILValue() const;
  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand getFirstUse() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getType() const;
  BRIDGED_INLINE Ownership getOwnership() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction SILUndef_getParentFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction PlaceholderValue_getParentFunction() const;

  bool findPointerEscape() const;
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
    AnyInteriorPointer,
    GuaranteedForwarding,
    EndBorrow,
    Reborrow
  };

  BRIDGED_INLINE bool isTypeDependent() const;
  BRIDGED_INLINE bool isLifetimeEnding() const;
  BRIDGED_INLINE bool canAcceptOwnership(BridgedValue::Ownership ownership) const;
  BRIDGED_INLINE bool isDeleted() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand getNextUse() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedValue getValue() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getUser() const;
  BRIDGED_INLINE OperandOwnership getOperandOwnership() const;
  void changeOwnership(BridgedValue::Ownership from, BridgedValue::Ownership to) const;
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

enum class BridgedMemoryBehavior {
  None,
  MayRead,
  MayWrite,
  MayReadWrite,
  MayHaveSideEffects
};

struct BridgedLocation {
  uint64_t storage[3];

  struct FilenameAndLocation {
    BridgedStringRef path;
    SwiftInt line;
    SwiftInt column;
  };

  BRIDGED_INLINE BridgedLocation(const swift::SILDebugLocation &loc);
  BRIDGED_INLINE const swift::SILDebugLocation &getLoc() const;

  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getAutogeneratedLocation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getCleanupLocation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation withScopeOf(BridgedLocation other) const;
  BRIDGED_INLINE bool hasValidLineNumber() const;
  BRIDGED_INLINE bool isAutoGenerated() const;
  BRIDGED_INLINE bool isInlined() const;
  BRIDGED_INLINE bool isEqualTo(BridgedLocation rhs) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSourceLoc getSourceLocation() const;
  BRIDGED_INLINE bool isFilenameAndLocation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE FilenameAndLocation getFilenameAndLocation() const;
  BRIDGED_INLINE bool hasSameSourceLocation(BridgedLocation rhs) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj getDecl() const;
  static BRIDGED_INLINE BridgedLocation fromNominalTypeDecl(BridgedDeclObj decl);
  static BRIDGED_INLINE BridgedLocation getArtificialUnreachableLocation();
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
    NoLocks = 2,
    NoRuntime = 3,
    NoExistentials = 4,
    NoObjCBridging = 5
  };

  enum class InlineStrategy {
    InlineDefault,
    NoInline,
    AlwaysInline
  };

  enum class ThunkKind {
    IsNotThunk,
    IsThunk,
    IsReabstractionThunk,
    IsSignatureOptimizedThunk
  };

  enum class SerializedKind {
    IsNotSerialized,
    IsSerialized,
    IsSerializedForPackage
  };

  enum class OptionalSourceFileKind {
    Library,
    Main,
    SIL,
    Interface,
    MacroExpansion,
    DefaultArgument, // must match swift::SourceFileKind::DefaultArgument
    None
  };

  SWIFT_NAME("init(obj:)")
  BridgedFunction(SwiftObject obj) : obj(obj) {}
  BridgedFunction() {}
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE swift::SILFunction * _Nonnull getFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getName() const;
  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getLocation() const;
  BRIDGED_INLINE bool isAccessor() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getAccessorName() const;
  BRIDGED_INLINE bool hasOwnership() const;
  BRIDGED_INLINE bool hasLoweredAddresses() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType getLoweredFunctionTypeInContext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGenericSignature getGenericSignature() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap getForwardingSubstitutionMap() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedASTType mapTypeIntoContext(BridgedASTType ty) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getFirstBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getLastBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclRef getDeclRef() const;
  BRIDGED_INLINE SwiftInt getNumIndirectFormalResults() const;
  BRIDGED_INLINE bool hasIndirectErrorResult() const;
  BRIDGED_INLINE SwiftInt getNumSILArguments() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getSILArgumentType(SwiftInt idx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getSILResultType() const;
  BRIDGED_INLINE bool isSwift51RuntimeAvailable() const;
  BRIDGED_INLINE bool isPossiblyUsedExternally() const;
  BRIDGED_INLINE bool isTransparent() const;
  BRIDGED_INLINE bool isAsync() const;
  BRIDGED_INLINE bool isGlobalInitFunction() const;
  BRIDGED_INLINE bool isGlobalInitOnceFunction() const;
  BRIDGED_INLINE bool isDestructor() const;
  BRIDGED_INLINE bool isGeneric() const;
  BRIDGED_INLINE bool hasSemanticsAttr(BridgedStringRef attrName) const;
  BRIDGED_INLINE bool hasUnsafeNonEscapableResult() const;
  BRIDGED_INLINE bool hasDynamicSelfMetadata() const;
  BRIDGED_INLINE EffectsKind getEffectAttribute() const;
  BRIDGED_INLINE PerformanceConstraints getPerformanceConstraints() const;
  BRIDGED_INLINE InlineStrategy getInlineStrategy() const;
  BRIDGED_INLINE SerializedKind getSerializedKind() const;
  BRIDGED_INLINE bool canBeInlinedIntoCaller(SerializedKind) const;
  BRIDGED_INLINE bool hasValidLinkageForFragileRef(SerializedKind) const;
  BRIDGED_INLINE ThunkKind isThunk() const;
  BRIDGED_INLINE void setThunk(ThunkKind) const;
  BRIDGED_INLINE bool needsStackProtection() const;
  BRIDGED_INLINE bool shouldOptimize() const;
  BRIDGED_INLINE bool isReferencedInModule() const;
  BRIDGED_INLINE bool wasDeserializedCanonical() const;
  BRIDGED_INLINE void setNeedStackProtection(bool needSP) const;
  BRIDGED_INLINE void setIsPerformanceConstraint(bool isPerfConstraint) const;
  BRIDGED_INLINE bool isResilientNominalDecl(BridgedDeclObj decl) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getLoweredType(BridgedASTType type, bool maximallyAbstracted) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getLoweredType(BridgedType type) const;
  BRIDGED_INLINE BridgedLinkage getLinkage() const;
  BRIDGED_INLINE void setLinkage(BridgedLinkage linkage) const;
  BRIDGED_INLINE void setIsSerialized(bool isSerialized) const;
  BRIDGED_INLINE bool conformanceMatchesActorIsolation(BridgedConformance conformance) const;
  BRIDGED_INLINE bool isSpecialization() const;
  bool isTrapNoReturn() const;
  bool isConvertPointerToPointerArgument() const;
  bool isAutodiffVJP() const;
  SwiftInt specializationLevel() const;
  SWIFT_IMPORT_UNSAFE BridgedSubstitutionMap getMethodSubstitutions(BridgedSubstitutionMap contextSubs,
                                                                    BridgedCanType selfType) const;
  BRIDGED_INLINE OptionalSourceFileKind getSourceFileKind() const;

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
  typedef bool (* _Nonnull ArgumentMayReadFn)(BridgedFunction, BridgedOperand, BridgedValue);
  typedef bool (* _Nonnull IsDeinitBarrierFn)(BridgedFunction);

  static void registerBridging(SwiftMetatype metatype,
              RegisterFn initFn, RegisterFn destroyFn,
              WriteFn writeFn, ParseFn parseFn,
              CopyEffectsFn copyEffectsFn,
              GetEffectInfoFn effectInfoFn,
              GetMemBehaviorFn memBehaviorFn,
              ArgumentMayReadFn argumentMayReadFn,
              IsDeinitBarrierFn isDeinitBarrierFn);
};

struct OptionalBridgedFunction {
  OptionalSwiftObject obj;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE swift::SILFunction * _Nullable getFunction() const;
};

struct BridgedGlobalVar {
  SwiftObject obj;

  BridgedGlobalVar(SwiftObject obj) : obj(obj) {}
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE swift::SILGlobalVariable * _Nonnull getGlobal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj getDecl() const;
  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef getName() const;
  BRIDGED_INLINE bool isLet() const;
  BRIDGED_INLINE void setLet(bool value) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getType() const;
  BRIDGED_INLINE BridgedLinkage getLinkage() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSourceLoc getSourceLocation() const;
  BRIDGED_INLINE bool isPossiblyUsedExternally() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getFirstStaticInitInst() const;
  bool canBeInitializedStatically() const;
  bool mustBeInitializedStatically() const;
  bool isConstValue() const;
};

struct OptionalBridgedGlobalVar {
  OptionalSwiftObject obj;
};

struct BridgedMultiValueResult {
  SwiftObject obj;

  BRIDGED_INLINE swift::MultipleValueInstructionResult * _Nonnull unbridged() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getParent() const;
  BRIDGED_INLINE SwiftInt getIndex() const;
};

struct BridgedSILTypeArray {
  BridgedArrayRef typeArray;

  SwiftInt getCount() const { return SwiftInt(typeArray.Length); }

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedType getAt(SwiftInt index) const;
};

struct BridgedGenericSpecializationInformation {
  const swift::GenericSpecializationInformation * _Nullable data = nullptr;
};

struct BridgedSILDebugVariable {
  uint64_t storage[16];

  BRIDGED_INLINE BridgedSILDebugVariable() {}
  BRIDGED_INLINE BridgedSILDebugVariable(const swift::SILDebugVariable &var);
  BRIDGED_INLINE BridgedSILDebugVariable(const BridgedSILDebugVariable &rhs);
  BRIDGED_INLINE ~BridgedSILDebugVariable();
  BRIDGED_INLINE BridgedSILDebugVariable &operator=(const BridgedSILDebugVariable &rhs);
  BRIDGED_INLINE swift::SILDebugVariable unbridge() const;
};

struct OptionalBridgedSILDebugVariable {
  BridgedSILDebugVariable debugVar;
  bool hasDebugVar = false;

  OptionalBridgedSILDebugVariable() {}
  OptionalBridgedSILDebugVariable(BridgedSILDebugVariable d) : debugVar(d), hasDebugVar(true) {}
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
  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getNext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getPrevious() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock getParent() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction getLastInstOfParent() const;
  BRIDGED_INLINE bool isDeleted() const;
  BRIDGED_INLINE bool isInStaticInitializer() const;
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
  bool maySynchronize() const;
  bool mayBeDeinitBarrierNotConsideringSideEffects() const;
  BRIDGED_INLINE bool shouldBeForwarding() const;

  // =========================================================================//
  //                   Generalized instruction subclasses
  // =========================================================================//
  
  BRIDGED_INLINE SwiftInt MultipleValueInstruction_getNumResults() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedMultiValueResult MultipleValueInstruction_getResult(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSuccessorArray TermInst_getSuccessors() const;

  // =========================================================================//
  //                         Instruction protocols
  // =========================================================================//

  BRIDGED_INLINE swift::ForwardingInstruction * _Nonnull getAsForwardingInstruction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedOperand ForwardingInst_singleForwardedOperand() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedOperandArray ForwardingInst_forwardedOperands() const;
  BRIDGED_INLINE BridgedValue::Ownership ForwardingInst_forwardingOwnership() const;
  BRIDGED_INLINE void ForwardingInst_setForwardingOwnership(BridgedValue::Ownership ownership) const;
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

  enum class MarkDependenceKind {
    Unresolved, Escaping, NonEscaping
  };

  struct KeyPathFunctionResults {
    enum { maxFunctions = 5 };
    BridgedFunction functions[maxFunctions];
    SwiftInt numFunctions;
  };

  enum class CastConsumptionKind {
    TakeAlways,
    TakeOnSuccess,
    CopyOnSuccess
  };

  struct CheckedCastInstOptions {
    uint8_t storage;
  };

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef CondFailInst_getMessage() const;
  BRIDGED_INLINE SwiftInt LoadInst_getLoadOwnership() const ;
  BRIDGED_INLINE bool LoadBorrowInst_isUnchecked() const ;
  BRIDGED_INLINE BuiltinValueKind BuiltinInst_getID() const;
  BRIDGED_INLINE IntrinsicID BuiltinInst_getIntrinsicID() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef BuiltinInst_getName() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap BuiltinInst_getSubstitutionMap() const;
  BRIDGED_INLINE bool PointerToAddressInst_isStrict() const;
  BRIDGED_INLINE bool PointerToAddressInst_isInvariant() const;
  BRIDGED_INLINE uint64_t PointerToAddressInst_getAlignment() const;
  BRIDGED_INLINE void PointerToAddressInst_setAlignment(uint64_t alignment) const;
  BRIDGED_INLINE bool AddressToPointerInst_needsStackProtection() const;
  BRIDGED_INLINE bool IndexAddrInst_needsStackProtection() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformanceArray InitExistentialRefInst_getConformances() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType InitExistentialRefInst_getFormalConcreteType() const;
  BRIDGED_INLINE bool OpenExistentialAddr_isImmutable() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGlobalVar GlobalAccessInst_getGlobal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGlobalVar AllocGlobalInst_getGlobal() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction FunctionRefBaseInst_getReferencedFunction() const;
  BRIDGED_INLINE BridgedOptionalInt IntegerLiteralInst_getValue() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef StringLiteralInst_getValue() const;
  BRIDGED_INLINE int StringLiteralInst_getEncoding() const;
  BRIDGED_INLINE SwiftInt TupleExtractInst_fieldIndex() const;
  BRIDGED_INLINE SwiftInt TupleElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE SwiftInt StructExtractInst_fieldIndex() const;
  BRIDGED_INLINE OptionalBridgedValue StructInst_getUniqueNonTrivialFieldValue() const;
  BRIDGED_INLINE SwiftInt StructElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE bool BeginBorrow_isLexical() const;
  BRIDGED_INLINE bool BeginBorrow_hasPointerEscape() const;
  BRIDGED_INLINE bool BeginBorrow_isFromVarDecl() const;
  BRIDGED_INLINE bool MoveValue_isLexical() const;
  BRIDGED_INLINE bool MoveValue_hasPointerEscape() const;
  BRIDGED_INLINE bool MoveValue_isFromVarDecl() const;

  BRIDGED_INLINE SwiftInt ProjectBoxInst_fieldIndex() const;
  BRIDGED_INLINE bool EndCOWMutationInst_doKeepUnique() const;
  BRIDGED_INLINE bool DestroyValueInst_isDeadEnd() const;
  BRIDGED_INLINE SwiftInt EnumInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt UncheckedEnumDataInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt InitEnumDataAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt UncheckedTakeEnumDataAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt InjectEnumAddrInst_caseIndex() const;
  BRIDGED_INLINE SwiftInt RefElementAddrInst_fieldIndex() const;
  BRIDGED_INLINE bool RefElementAddrInst_fieldIsLet() const;
  BRIDGED_INLINE bool RefElementAddrInst_isImmutable() const;
  BRIDGED_INLINE void RefElementAddrInst_setImmutable(bool isImmutable) const;
  BRIDGED_INLINE bool RefTailAddrInst_isImmutable() const;
  BRIDGED_INLINE SwiftInt PartialApplyInst_numArguments() const;
  BRIDGED_INLINE SwiftInt ApplyInst_numArguments() const;
  BRIDGED_INLINE bool ApplyInst_getNonThrowing() const;
  BRIDGED_INLINE bool ApplyInst_getNonAsync() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGenericSpecializationInformation ApplyInst_getSpecializationInfo() const;
  BRIDGED_INLINE bool TryApplyInst_getNonAsync() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedGenericSpecializationInformation TryApplyInst_getSpecializationInfo() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclRef ClassMethodInst_getMember() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclRef WitnessMethodInst_getMember() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType WitnessMethodInst_getLookupType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj WitnessMethodInst_getLookupProtocol() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance WitnessMethodInst_getConformance() const;
  BRIDGED_INLINE SwiftInt ObjectInst_getNumBaseElements() const;
  BRIDGED_INLINE SwiftInt PartialApply_getCalleeArgIndexOfFirstAppliedArg() const;
  BRIDGED_INLINE bool PartialApplyInst_isOnStack() const;
  BRIDGED_INLINE bool PartialApplyInst_hasUnknownResultIsolation() const;
  BRIDGED_INLINE bool AllocStackInst_hasDynamicLifetime() const;
  BRIDGED_INLINE bool AllocStackInst_isFromVarDecl() const;
  BRIDGED_INLINE bool AllocStackInst_usesMoveableValueDebugInfo() const;
  BRIDGED_INLINE bool AllocStackInst_isLexical() const;
  BRIDGED_INLINE bool AllocRefInstBase_isObjc() const;
  BRIDGED_INLINE bool AllocRefInstBase_canAllocOnStack() const;
  BRIDGED_INLINE SwiftInt AllocRefInstBase_getNumTailTypes() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSILTypeArray AllocRefInstBase_getTailAllocatedTypes() const;
  BRIDGED_INLINE bool AllocRefDynamicInst_isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType() const;
  BRIDGED_INLINE SwiftInt BeginApplyInst_numArguments() const;
  BRIDGED_INLINE bool BeginApplyInst_isCalleeAllocated() const;
  BRIDGED_INLINE SwiftInt TryApplyInst_numArguments() const;
  BRIDGED_INLINE BridgedArgumentConvention YieldInst_getConvention(BridgedOperand forOperand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock BranchInst_getTargetBlock() const;
  BRIDGED_INLINE SwiftInt SwitchEnumInst_getNumCases() const;
  BRIDGED_INLINE SwiftInt SwitchEnumInst_getCaseIndex(SwiftInt idx) const;
  BRIDGED_INLINE SwiftInt StoreInst_getStoreOwnership() const;
  BRIDGED_INLINE SwiftInt AssignInst_getAssignOwnership() const;
  BRIDGED_INLINE MarkDependenceKind MarkDependenceInst_dependenceKind() const;
  BRIDGED_INLINE void MarkDependenceInstruction_resolveToNonEscaping() const;
  BRIDGED_INLINE void MarkDependenceInstruction_settleToEscaping() const;
  BRIDGED_INLINE MarkDependenceKind MarkDependenceAddrInst_dependenceKind() const;
  BRIDGED_INLINE SwiftInt BeginAccessInst_getAccessKind() const;
  BRIDGED_INLINE bool BeginAccessInst_isStatic() const;
  BRIDGED_INLINE bool BeginAccessInst_isUnsafe() const;
  BRIDGED_INLINE void BeginAccess_setAccessKind(SwiftInt accessKind) const;
  BRIDGED_INLINE bool CopyAddrInst_isTakeOfSrc() const;
  BRIDGED_INLINE bool CopyAddrInst_isInitializationOfDest() const;
  BRIDGED_INLINE void CopyAddrInst_setIsTakeOfSrc(bool isTakeOfSrc) const;
  BRIDGED_INLINE void CopyAddrInst_setIsInitializationOfDest(bool isInitializationOfDest) const;
  BRIDGED_INLINE bool ExplicitCopyAddrInst_isTakeOfSrc() const;
  BRIDGED_INLINE bool ExplicitCopyAddrInst_isInitializationOfDest() const;
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
  BRIDGED_INLINE void GlobalAddrInst_clearToken() const;
  BRIDGED_INLINE bool GlobalValueInst_isBare() const;
  BRIDGED_INLINE void GlobalValueInst_setIsBare() const;
  BRIDGED_INLINE void LoadInst_setOwnership(SwiftInt ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType UnconditionalCheckedCast_getSourceFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType UnconditionalCheckedCast_getTargetFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE CheckedCastInstOptions
      UnconditionalCheckedCast_getCheckedCastOptions() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType UnconditionalCheckedCastAddr_getSourceFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType UnconditionalCheckedCastAddr_getTargetFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE CheckedCastInstOptions
      UnconditionalCheckedCastAddr_getCheckedCastOptions() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastBranch_getSuccessBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastBranch_getFailureBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE CheckedCastInstOptions
      CheckedCastBranch_getCheckedCastOptions() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType CheckedCastAddrBranch_getSourceFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType CheckedCastAddrBranch_getTargetFormalType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastAddrBranch_getSuccessBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock CheckedCastAddrBranch_getFailureBlock() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE CheckedCastInstOptions
      CheckedCastAddrBranch_getCheckedCastOptions() const;
  BRIDGED_INLINE void CheckedCastBranch_updateSourceFormalTypeFromOperandLoweredType() const;
  BRIDGED_INLINE CastConsumptionKind CheckedCastAddrBranch_getConsumptionKind() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap ApplySite_getSubstitutionMap() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType ApplySite_getSubstitutedCalleeType() const;
  BRIDGED_INLINE SwiftInt ApplySite_getNumArguments() const;
  BRIDGED_INLINE bool ApplySite_isCalleeNoReturn() const;
  BRIDGED_INLINE SwiftInt FullApplySite_numIndirectResultArguments() const;
  BRIDGED_INLINE bool ConvertFunctionInst_withoutActuallyEscaping() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType TypeValueInst_getParamType() const;

  // =========================================================================//
  //                   VarDeclInst and DebugVariableInst
  // =========================================================================//

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj
  DebugValue_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj
  AllocStack_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj
  AllocBox_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj
  GlobalAddr_getDecl() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj
  RefElementAddr_getDecl() const;

  BRIDGED_INLINE bool DebugValue_hasVarInfo() const;
  BRIDGED_INLINE BridgedSILDebugVariable DebugValue_getVarInfo() const;

  BRIDGED_INLINE bool AllocStack_hasVarInfo() const;
  BRIDGED_INLINE BridgedSILDebugVariable AllocStack_getVarInfo() const;

  BRIDGED_INLINE bool AllocBox_hasVarInfo() const;
  BRIDGED_INLINE BridgedSILDebugVariable AllocBox_getVarInfo() const;
};

struct OptionalBridgedInstruction {
  OptionalSwiftObject obj;

  BRIDGED_INLINE swift::SILInstruction * _Nullable unbridged() const;

  OptionalBridgedInstruction() : obj(nullptr) {}
  OptionalBridgedInstruction(OptionalSwiftObject obj) : obj(obj) {}
};

struct BridgedArgument {
  SwiftObject obj;

  BRIDGED_INLINE swift::SILArgument * _Nonnull getArgument() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedBasicBlock getParent() const;
  BRIDGED_INLINE bool isReborrow() const;
  BRIDGED_INLINE bool FunctionArgument_isLexical() const;
  BRIDGED_INLINE bool FunctionArgument_isClosureCapture() const;
  BRIDGED_INLINE void setReborrow(bool reborrow) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj getDecl() const;
  BRIDGED_INLINE void copyFlags(BridgedArgument fromArgument) const;
};

struct OptionalBridgedArgument {
  OptionalSwiftObject obj;

  BRIDGED_INLINE swift::SILArgument * _Nullable unbridged() const;
};

struct OptionalBridgedBasicBlock {
  OptionalSwiftObject obj;

  BRIDGED_INLINE swift::SILBasicBlock * _Nullable unbridged() const;
};

struct BridgedBasicBlock {
  SwiftObject obj;

  BridgedBasicBlock(SwiftObject obj) : obj(obj) {
  }

  BRIDGED_INLINE BridgedBasicBlock(swift::SILBasicBlock * _Nonnull block);
  BRIDGED_INLINE swift::SILBasicBlock * _Nonnull unbridged() const;

  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getNext() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedBasicBlock getPrevious() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getFunction() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getFirstInst() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedInstruction getLastInst() const;
  BRIDGED_INLINE SwiftInt getNumArguments() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedArgument getArgument(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedArgument addBlockArgument(BridgedType type, BridgedValue::Ownership ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedArgument addFunctionArgument(BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedArgument insertFunctionArgument(SwiftInt atPosition, BridgedType type,
                                                                            BridgedValue::Ownership ownership,
                                                                            OptionalBridgedDeclObj decl) const;
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

struct BridgedDeclRef {
  uint64_t storage[3];

  BRIDGED_INLINE BridgedDeclRef(swift::SILDeclRef declRef);
  BRIDGED_INLINE swift::SILDeclRef unbridged() const;

  BRIDGED_INLINE bool isEqualTo(BridgedDeclRef rhs) const;
  BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedLocation getLocation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getDecl() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDiagnosticArgument asDiagnosticArgument() const;
};

struct BridgedVTableEntry {
  uint64_t storage[5];

  enum class Kind {
    Normal,
    Inherited,
    Override
  };

  BridgedVTableEntry() : storage{0, 0, 0, 0, 0} {};
  BRIDGED_INLINE BridgedVTableEntry(const swift::SILVTableEntry &entry);
  BRIDGED_INLINE const swift::SILVTableEntry &unbridged() const;

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE Kind getKind() const;
  BRIDGED_INLINE bool isNonOverridden() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclRef getMethodDecl() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedFunction getImplementation() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedVTableEntry create(Kind kind, bool nonOverridden,
                                   BridgedDeclRef methodDecl, BridgedFunction implementation);
};

struct OptionalBridgedVTableEntry {
  BridgedVTableEntry entry;
  bool hasEntry = false;

  OptionalBridgedVTableEntry() {}
  OptionalBridgedVTableEntry(BridgedVTableEntry e) : entry(e), hasEntry(true) {}
};

struct BridgedVTableEntryArray {
  BridgedVTableEntry base;
  SwiftInt count;
};

struct BridgedVTable {
  swift::SILVTable * _Nonnull vTable;

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE SwiftInt getNumEntries() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedVTableEntry getEntry(SwiftInt index) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getClass() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedVTableEntry lookupMethod(BridgedDeclRef member) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedType getSpecializedClassType() const;
  BRIDGED_INLINE void replaceEntries(BridgedArrayRef bridgedEntries) const;
};

struct OptionalBridgedVTable {
  swift::SILVTable * _Nullable table;
};

struct BridgedConstExprFunctionState {
  swift::ConstExprFunctionState * _Nonnull state;
  swift::SymbolicValueBumpAllocator * _Nonnull allocator;
  swift::ConstExprEvaluator * _Nonnull constantEvaluator;
  unsigned int * _Nonnull numEvaluatedSILInstructions;
  
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedConstExprFunctionState create();
  
  BRIDGED_INLINE
  bool isConstantValue(BridgedValue value);
  
  BRIDGED_INLINE
  void deinitialize();
};

struct BridgedWitnessTableEntry {
  uint64_t storage[5];

  enum class Kind {
    invalid,
    method,
    associatedType,
    associatedConformance,
    baseProtocol
  };

#ifdef USED_IN_CPP_SOURCE
  static BridgedWitnessTableEntry bridge(const swift::SILWitnessTable::Entry &entry) {
    BridgedWitnessTableEntry bridgedEntry;
    *reinterpret_cast<swift::SILWitnessTable::Entry *>(&bridgedEntry.storage) = entry;
    return bridgedEntry;
  }

  const swift::SILWitnessTable::Entry &unbridged() const {
    return *reinterpret_cast<const swift::SILWitnessTable::Entry *>(&storage);
  }
#endif

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE Kind getKind() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclRef getMethodRequirement() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedFunction getMethodWitness() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getAssociatedTypeRequirement() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType getAssociatedTypeWitness() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType getAssociatedConformanceRequirement() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance getAssociatedConformanceWitness() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj getBaseProtocolRequirement() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance getBaseProtocolWitness() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedWitnessTableEntry createInvalid();
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedWitnessTableEntry createMethod(BridgedDeclRef requirement,
                                               OptionalBridgedFunction witness);
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedWitnessTableEntry createAssociatedType(BridgedDeclObj requirement,
                                                       BridgedCanType witness);
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedWitnessTableEntry createAssociatedConformance(BridgedCanType requirement,
                                                              BridgedConformance witness);
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  static BridgedWitnessTableEntry createBaseProtocol(BridgedDeclObj requirement,
                                                     BridgedConformance witness);
};

struct BridgedWitnessTable {
  const swift::SILWitnessTable * _Nonnull table;

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE SwiftInt getNumEntries() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTableEntry getEntry(SwiftInt index) const;
  BRIDGED_INLINE bool isDeclaration() const;
  BRIDGED_INLINE bool isSpecialized() const;
};

struct OptionalBridgedWitnessTable {
  const swift::SILWitnessTable * _Nullable table;
};

struct BridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nonnull table;

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE SwiftInt getNumEntries() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedWitnessTableEntry getEntry(SwiftInt index) const;
};

struct OptionalBridgedDefaultWitnessTable {
  const swift::SILDefaultWitnessTable * _Nullable table;
};

struct BridgedBuilder{

  enum class InsertAt {
    beforeInst, endOfBlock, startOfFunction, intoGlobal
  } insertAt;

  SwiftObject insertionObj;
  BridgedLocation loc;

  BRIDGED_INLINE swift::SILBuilder unbridged() const;
  BRIDGED_INLINE swift::SILLocation regularLoc() const;
  BRIDGED_INLINE swift::SILLocation returnLoc() const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBuiltin(BridgedStringRef name,
                                                                      BridgedType type,
                                                                      BridgedSubstitutionMap subs,
                                                                      BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBuiltinBinaryFunction(BridgedStringRef name,
                                          BridgedType operandType, BridgedType resultType,
                                          BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCondFail(BridgedValue condition,
                                                                       BridgedStringRef message) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createIntegerLiteral(BridgedType type, SwiftInt value) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createAllocRef(BridgedType type,
    bool objc, bool canAllocOnStack, bool isBare,
    BridgedSILTypeArray elementTypes, BridgedValueArray elementCountOperands) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction
  createAllocStack(BridgedType type, OptionalBridgedSILDebugVariable debugVar,
                   bool hasDynamicLifetime, bool isLexical, bool isFromVarDecl, bool wasMoved) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createAllocVector(BridgedValue capacity,
                                                                          BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDeallocStack(BridgedValue operand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDeallocStackRef(BridgedValue operand) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createAddressToPointer(BridgedValue address,
                                                                               BridgedType pointerTy,
                                                                               bool needsStackProtection) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createPointerToAddress(BridgedValue pointer,
                                                                               BridgedType addressTy,
                                                                               bool isStrict,
                                                                               bool isInvariant,
                                                                               uint64_t alignment) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createIndexAddr(BridgedValue base,
                                                                        BridgedValue index,
                                                                        bool needsStackProtection) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedRefCast(BridgedValue op,
                                                                               BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedAddrCast(BridgedValue op,
                                                                                BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUpcast(BridgedValue op, BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCheckedCastAddrBranch(
      BridgedValue source, BridgedCanType sourceFormalType,
      BridgedValue destination, BridgedCanType targetFormalType,
      BridgedInstruction::CheckedCastInstOptions options,
      BridgedInstruction::CastConsumptionKind consumptionKind,
      BridgedBasicBlock successBlock, BridgedBasicBlock failureBlock) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnconditionalCheckedCastAddr(
        BridgedInstruction::CheckedCastInstOptions options,
        BridgedValue source, BridgedCanType sourceFormalType,
        BridgedValue destination, BridgedCanType targetFormalType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedOwnershipConversion(
        BridgedValue op, BridgedValue::Ownership ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createLoad(BridgedValue op, SwiftInt ownership) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createLoadBorrow(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBeginDeallocRef(BridgedValue reference,
                                                                              BridgedValue allocation) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndInitLetRef(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction
  createRetainValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction
  createReleaseValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStrongRetain(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createStrongRelease(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnownedRetain(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnownedRelease(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createFunctionRef(BridgedFunction function) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCopyValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBeginBorrow(BridgedValue op,
                                                                          bool isLexical,
                                                                          bool hasPointerEscape,
                                                                          bool isFromVarDecl) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBorrowedFrom(BridgedValue borrowedValue,
                                                                           BridgedValueArray enclosingValues) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndBorrow(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createCopyAddr(BridgedValue from, BridgedValue to,
                                          bool takeSource, bool initializeDest) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestroyValue(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDestroyAddr(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndLifetime(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDebugValue(BridgedValue src,
                                                                         BridgedSILDebugVariable var) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createDebugStep() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createApply(BridgedValue function,
                                          BridgedSubstitutionMap subMap,
                                          BridgedValueArray arguments, bool isNonThrowing, bool isNonAsync,
                                          BridgedGenericSpecializationInformation specInfo) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createTryApply(BridgedValue function,
                                          BridgedSubstitutionMap subMap,
                                          BridgedValueArray arguments,
                                          BridgedBasicBlock normalBB, BridgedBasicBlock errorBB,
                                          bool isNonAsync,
                                          BridgedGenericSpecializationInformation specInfo) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createWitnessMethod(BridgedCanType lookupType,
                                          BridgedConformance conformance,
                                          BridgedDeclRef member, BridgedType methodType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createReturn(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createThrow(BridgedValue op) const;
  SWIFT_IMPORT_UNSAFE BridgedInstruction createSwitchEnumInst(BridgedValue enumVal,
                                          OptionalBridgedBasicBlock defaultBlock,
                                          const void * _Nullable enumCases, SwiftInt numEnumCases) const;
  SWIFT_IMPORT_UNSAFE BridgedInstruction createSwitchEnumAddrInst(BridgedValue enumAddr,
                                          OptionalBridgedBasicBlock defaultBlock,
                                          const void * _Nullable enumCases, SwiftInt numEnumCases) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedEnumData(BridgedValue enumVal,
                                          SwiftInt caseIdx, BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUncheckedTakeEnumDataAddr(BridgedValue enumAddr,
                                                                                        SwiftInt caseIdx) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createInitEnumDataAddr(BridgedValue enumAddr,
                                                                               SwiftInt caseIdx,
                                                                               BridgedType type) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEnum(SwiftInt caseIdx, OptionalBridgedValue payload,
                                          BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createThinToThickFunction(BridgedValue fn,
                                                                                  BridgedType resultType) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createPartialApply(BridgedValue fn,
                                                                           BridgedValueArray bridgedCapturedArgs,
                                                                           BridgedArgumentConvention calleeConvention,
                                                                           BridgedSubstitutionMap bridgedSubstitutionMap = BridgedSubstitutionMap(),
                                                                           bool hasUnknownIsolation = true,
                                                                           bool isOnStack = false) const;                                                                                  
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createBranch(BridgedBasicBlock destBlock,
                                                                     BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createUnreachable() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createObject(BridgedType type, BridgedValueArray arguments,
                                                                     SwiftInt numBaseElements) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createVector(BridgedValueArray arguments) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createVectorBaseAddr(BridgedValue vector) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createGlobalAddr(BridgedGlobalVar global,
                                                                         OptionalBridgedValue dependencyToken) const;
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
                                          BridgedCanType formalConcreteType,
                                          BridgedConformanceArray conformances) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createInitExistentialMetatype(BridgedValue metatype,
                                          BridgedType existentialType,
                                          BridgedConformanceArray conformances) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createMetatype(BridgedCanType instanceType,
                                          BridgedASTType::MetatypeRepresentation representation) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndCOWMutation(BridgedValue instance,
                                                                             bool keepUnique) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndCOWMutationAddr(BridgedValue instance) const;                                                                             
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createMarkDependence(
    BridgedValue value, BridgedValue base, BridgedInstruction::MarkDependenceKind dependenceKind) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createMarkDependenceAddr(
    BridgedValue value, BridgedValue base, BridgedInstruction::MarkDependenceKind dependenceKind) const;
    
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndAccess(BridgedValue value) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createEndApply(BridgedValue value) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createAbortApply(BridgedValue value) const;

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createConvertFunction(BridgedValue originalFunction, BridgedType resultType, bool withoutActuallyEscaping) const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedInstruction createConvertEscapeToNoEscape(BridgedValue originalFunction, BridgedType resultType, bool isLifetimeGuaranteed) const;

  SWIFT_IMPORT_UNSAFE void destroyCapturedArgs(BridgedInstruction partialApply) const;
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
    effectsChanged,
    functionTablesChanged
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

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file. This is because they rely on C++ headers that
// we don't want to pull in when using "pure bridging mode".
#include "SILBridgingImpl.h"
#endif

#endif
