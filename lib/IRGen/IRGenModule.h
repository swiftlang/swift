//===--- IRGenModule.h - Swift Global IR Generation Module ------*- C++ -*-===//
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
//
// This file defines the interface used 
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENMODULE_H
#define SWIFT_IRGEN_IRGENMODULE_H

#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ClusteredBitVector.h"
#include "swift/Basic/SuccessorMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/IR/Attributes.h"
#include "llvm/Target/TargetMachine.h"
#include "IRGen.h"
#include "SwiftTargetInfo.h"
#include "ValueWitness.h"

#include <atomic>

namespace llvm {
  class Constant;
  class DataLayout;
  class Function;
  class FunctionType;
  class GlobalVariable;
  class IntegerType;
  class LLVMContext;
  class MDNode;
  class Metadata;
  class Module;
  class PointerType;
  class StructType;
  class StringRef;
  class Type;
  class AttributeSet;
}
namespace clang {
  class ASTContext;
  template <class> class CanQual;
  class CodeGenerator;
  class Decl;
  class GlobalDecl;
  class Type;
  namespace CodeGen {
    class CGFunctionInfo;
  }
}

namespace swift {
  class GenericSignatureBuilder;
  class ASTContext;
  class BraceStmt;
  class CanType;
  class ClassDecl;
  class ConstructorDecl;
  class Decl;
  class DestructorDecl;
  class ExtensionDecl;
  class FuncDecl;
  class LinkLibrary;
  class SILFunction;
  class EnumElementDecl;
  class EnumDecl;
  class IRGenOptions;
  class NormalProtocolConformance;
  class ProtocolConformance;
  class ProtocolCompositionType;
  class ProtocolDecl;
  struct SILDeclRef;
  class SILGlobalVariable;
  class SILModule;
  class SILType;
  class SILWitnessTable;
  class SourceLoc;
  class SourceFile;
  class StructDecl;
  class Type;
  class TypeAliasDecl;
  class TypeDecl;
  class ValueDecl;
  class VarDecl;

namespace Lowering {
  class TypeConverter;
}

namespace irgen {
  class Address;
  class ClangTypeConverter;
  class DebugTypeInfo;
  class EnumImplStrategy;
  class ExplosionSchema;
  class FixedTypeInfo;
  class ForeignFunctionInfo;
  class FormalType;
  class HeapLayout;
  class IRGenDebugInfo;
  class IRGenFunction;
  class LinkEntity;
  class LoadableTypeInfo;
  class ProtocolInfo;
  class TypeConverter;
  class TypeInfo;
  enum class ValueWitness : unsigned;
  enum class ReferenceCounting : unsigned char;

class IRGenModule;

/// A type descriptor for a field type accessor.
class FieldTypeInfo {
  llvm::PointerIntPair<CanType, 2, unsigned> Info;
  /// Bits in the "int" part of the Info pair.
  enum : unsigned {
    /// Flag indicates that the case is indirectly stored in a box.
    Indirect = 1,
    /// Indicates a weak optional reference
    Weak = 2,
  };

  static unsigned getFlags(bool indirect, bool weak) {
    return (indirect ? Indirect : 0)
         | (weak ? Weak : 0);
    //   | (blah ? Blah : 0) ...
  }

public:
  FieldTypeInfo(CanType type, bool indirect, bool weak)
    : Info(type, getFlags(indirect, weak))
  {}

  CanType getType() const { return Info.getPointer(); }
  bool isIndirect() const { return Info.getInt() & Indirect; }
  bool isWeak() const { return Info.getInt() & Weak; }
  bool hasFlags() const { return Info.getInt() != 0; }
};

/// The principal singleton which manages all of IR generation.
///
/// The IRGenerator delegates the emission of different top-level entities
/// to different instances of IRGenModule, each of which creates a different
/// llvm::Module.
///
/// In single-threaded compilation, the IRGenerator creates only a single
/// IRGenModule. In multi-threaded compilation, it contains multiple
/// IRGenModules - one for each LLVM module (= one for each input/output file).
class IRGenerator {
public:
  IRGenOptions &Opts;

  SILModule &SIL;

private:
  llvm::DenseMap<SourceFile *, IRGenModule *> GenModules;
  
  // Stores the IGM from which a function is referenced the first time.
  // It is used if a function has no source-file association.
  llvm::DenseMap<SILFunction *, IRGenModule *> DefaultIGMForFunction;
  
  // The IGM of the first source file.
  IRGenModule *PrimaryIGM = nullptr;

  // The current IGM for which IR is generated.
  IRGenModule *CurrentIGM = nullptr;

  /// The set of type metadata that have been enqueue for lazy emission.
  llvm::SmallPtrSet<CanType, 4> LazilyEmittedTypeMetadata;
  
  /// The queue of lazy type metadata to emit.
  llvm::SmallVector<CanType, 4> LazyTypeMetadata;
  
  llvm::SmallPtrSet<SILFunction*, 4> LazilyEmittedFunctions;

  struct LazyFieldTypeAccessor {
    NominalTypeDecl *type;
    std::vector<FieldTypeInfo> fieldTypes;
    llvm::Function *fn;
    IRGenModule *IGM;
  };
  
  /// Field type accessors we need to emit.
  llvm::SmallVector<LazyFieldTypeAccessor, 4> LazyFieldTypeAccessors;

  /// SIL functions that we need to emit lazily.
  llvm::SmallVector<SILFunction*, 4> LazyFunctionDefinitions;

  /// The order in which all the SIL function definitions should
  /// appear in the translation unit.
  llvm::DenseMap<SILFunction*, unsigned> FunctionOrder;

  /// The queue of IRGenModules for multi-threaded compilation.
  SmallVector<IRGenModule *, 8> Queue;

  std::atomic<int> QueueIndex;
  
  friend class CurrentIGMPtr;  
public:
  explicit IRGenerator(IRGenOptions &opts, SILModule &module);

  /// Attempt to create an llvm::TargetMachine for the current target.
  std::unique_ptr<llvm::TargetMachine> createTargetMachine();

  /// Add an IRGenModule for a source file.
  /// Should only be called from IRGenModule's constructor.
  void addGenModule(SourceFile *SF, IRGenModule *IGM);
  
  /// Get an IRGenModule for a source file.
  IRGenModule *getGenModule(SourceFile *SF) {
    IRGenModule *IGM = GenModules[SF];
    assert(IGM);
    return IGM;
  }
  
  /// Get an IRGenModule for a declaration context.
  /// Returns the IRGenModule of the containing source file, or if this cannot
  /// be determined, returns the primary IRGenModule.
  IRGenModule *getGenModule(DeclContext *ctxt);

  /// Get an IRGenModule for a function.
  /// Returns the IRGenModule of the containing source file, or if this cannot
  /// be determined, returns the IGM from which the function is referenced the
  /// first time.
  IRGenModule *getGenModule(SILFunction *f);

  /// Returns the primary IRGenModule. This is the first added IRGenModule.
  /// It is used for everything which cannot be correlated to a specific source
  /// file. And of course, in single-threaded compilation there is only the
  /// primary IRGenModule.
  IRGenModule *getPrimaryIGM() const {
    assert(PrimaryIGM);
    return PrimaryIGM;
  }
  
  bool hasMultipleIGMs() const { return GenModules.size() >= 2; }
  
  llvm::DenseMap<SourceFile *, IRGenModule *>::iterator begin() {
    return GenModules.begin();
  }
  
  llvm::DenseMap<SourceFile *, IRGenModule *>::iterator end() {
    return GenModules.end();
  }
  
  /// Emit functions, variables and tables which are needed anyway, e.g. because
  /// they are externally visible.
  void emitGlobalTopLevel();

  /// Emit the protocol conformance records needed by each IR module.
  void emitProtocolConformances();

  /// Emit type metadata records for types without explicit protocol conformance.
  void emitTypeMetadataRecords();

  /// Emit reflection metadata records for builtin and imported types referenced
  /// from this module.
  void emitBuiltinReflectionMetadata();

  /// Emit a symbol identifying the reflection metadata version.
  void emitReflectionMetadataVersion();

  /// Emit everything which is reachable from already emitted IR.
  void emitLazyDefinitions();
  
  void addLazyFunction(SILFunction *f) {
    // Add it to the queue if it hasn't already been put there.
    if (LazilyEmittedFunctions.insert(f).second) {
      LazyFunctionDefinitions.push_back(f);
      DefaultIGMForFunction[f] = CurrentIGM;
    }
  }
  
  void addLazyTypeMetadata(CanType type) {
    // Add it to the queue if it hasn't already been put there.
    if (LazilyEmittedTypeMetadata.insert(type).second)
      LazyTypeMetadata.push_back(type);
  }
  
  void addLazyFieldTypeAccessor(NominalTypeDecl *type,
                                ArrayRef<FieldTypeInfo> fieldTypes,
                                llvm::Function *fn,
                                IRGenModule *IGM) {
    LazyFieldTypeAccessors.push_back({type,
                                      {fieldTypes.begin(), fieldTypes.end()},
                                      fn, IGM});
  }
  
  unsigned getFunctionOrder(SILFunction *F) {
    auto it = FunctionOrder.find(F);
    assert(it != FunctionOrder.end() &&
           "no order number for SIL function definition?");
    return it->second;
  }
  
  /// In multi-threaded compilation fetch the next IRGenModule from the queue.
  IRGenModule *fetchFromQueue() {
    int idx = QueueIndex++;
    if (idx < (int)Queue.size()) {
      return Queue[idx];
    }
    return nullptr;
  }
};

class ConstantReference {
public:
  enum Directness : bool { Direct, Indirect };
private:
  llvm::PointerIntPair<llvm::Constant *, 1, Directness> ValueAndIsIndirect;
public:
  ConstantReference() {}
  ConstantReference(llvm::Constant *value, Directness isIndirect)
    : ValueAndIsIndirect(value, isIndirect) {}

  Directness isIndirect() const { return ValueAndIsIndirect.getInt(); }
  llvm::Constant *getValue() const { return ValueAndIsIndirect.getPointer(); }

  llvm::Constant *getDirectValue() const {
    assert(!isIndirect());
    return getValue();
  }

  explicit operator bool() const {
    return ValueAndIsIndirect.getPointer() != nullptr;
  }
};

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenModule {
public:
  // The ABI version of the Swift data generated by this file.
  static const uint32_t swiftVersion = 4;

  IRGenerator &IRGen;
  ASTContext &Context;
  std::unique_ptr<clang::CodeGenerator> ClangCodeGen;
  llvm::Module &Module;
  llvm::LLVMContext &LLVMContext;
  const llvm::DataLayout DataLayout;
  const llvm::Triple &Triple;
  std::unique_ptr<llvm::TargetMachine> TargetMachine;
  ModuleDecl *getSwiftModule() const;
  Lowering::TypeConverter &getSILTypes() const;
  SILModule &getSILModule() const { return IRGen.SIL; }
  SILModuleConventions silConv;

  llvm::SmallString<128> OutputFilename;

#ifndef NDEBUG
  // Used for testing ConformanceCollector.
  ConformanceCollector EligibleConfs;
  SILInstruction *CurrentInst = nullptr;
  SILWitnessTable *CurrentWitnessTable = nullptr;
#endif

  /// Order dependency -- TargetInfo must be initialized after Opts.
  const SwiftTargetInfo TargetInfo;
  /// Holds lexical scope info, etc. Is a nullptr if we compile without -g.
  IRGenDebugInfo *DebugInfo;

  /// A global variable which stores the hash of the module. Used for
  /// incremental compilation.
  llvm::GlobalVariable *ModuleHash;

  /// Does the current target require Objective-C interoperation?
  bool ObjCInterop = true;

  /// Should we add value names to local IR values?
  bool EnableValueNames = false;

  // Is swifterror returned in a register by the target ABI.
  bool IsSwiftErrorInRegister;

  llvm::Type *VoidTy;                  /// void (usually {})
  llvm::IntegerType *Int1Ty;           /// i1
  llvm::IntegerType *Int8Ty;           /// i8
  llvm::IntegerType *Int16Ty;          /// i16
  union {
    llvm::IntegerType *Int32Ty;          /// i32
    llvm::IntegerType *RelativeAddressTy;
  };
  llvm::IntegerType *Int64Ty;          /// i64
  union {
    llvm::IntegerType *SizeTy;         /// usually i32 or i64
    llvm::IntegerType *IntPtrTy;
    llvm::IntegerType *MetadataKindTy;
    llvm::IntegerType *OnceTy;
    llvm::IntegerType *FarRelativeAddressTy;
  };
  llvm::IntegerType *ObjCBoolTy;       /// i8 or i1
  union {
    llvm::PointerType *Int8PtrTy;      /// i8*
    llvm::PointerType *WitnessTableTy;
    llvm::PointerType *ObjCSELTy;
    llvm::PointerType *FunctionPtrTy;
    llvm::PointerType *CaptureDescriptorPtrTy;
  };
  union {
    llvm::PointerType *Int8PtrPtrTy;   /// i8**
    llvm::PointerType *WitnessTablePtrTy;
  };
  llvm::StructType *RefCountedStructTy;/// %swift.refcounted = type { ... }
  llvm::PointerType *RefCountedPtrTy;  /// %swift.refcounted*
  llvm::PointerType *WeakReferencePtrTy;/// %swift.weak_reference*
  llvm::PointerType *UnownedReferencePtrTy;/// %swift.unowned_reference*
  llvm::Constant *RefCountedNull;      /// %swift.refcounted* null
  llvm::StructType *FunctionPairTy;    /// { i8*, %swift.refcounted* }
  llvm::StructType *WitnessFunctionPairTy;    /// { i8*, %witness.table* }
  llvm::FunctionType *DeallocatingDtorTy; /// void (%swift.refcounted*)
  llvm::StructType *TypeMetadataStructTy; /// %swift.type = type { ... }
  llvm::PointerType *TypeMetadataPtrTy;/// %swift.type*
  llvm::PointerType *TupleTypeMetadataPtrTy; /// %swift.tuple_type*
  llvm::StructType *FullHeapMetadataStructTy; /// %swift.full_heapmetadata = type { ... }
  llvm::PointerType *FullHeapMetadataPtrTy;/// %swift.full_heapmetadata*
  llvm::StructType *FullBoxMetadataStructTy; /// %swift.full_boxmetadata = type { ... }
  llvm::PointerType *FullBoxMetadataPtrTy;/// %swift.full_boxmetadata*
  llvm::StructType *TypeMetadataPatternStructTy;/// %swift.type_pattern = type { ... }
  llvm::PointerType *TypeMetadataPatternPtrTy;/// %swift.type_pattern*
  llvm::StructType *FullTypeMetadataStructTy; /// %swift.full_type = type { ... }
  llvm::PointerType *FullTypeMetadataPtrTy;/// %swift.full_type*
  llvm::StructType *ProtocolDescriptorStructTy; /// %swift.protocol = type { ... }
  llvm::PointerType *ProtocolDescriptorPtrTy; /// %swift.protocol*
  union {
    llvm::PointerType *ObjCPtrTy;        /// %objc_object*
    llvm::PointerType *UnknownRefCountedPtrTy;
  };
  llvm::PointerType *BridgeObjectPtrTy; /// %swift.bridge*
  llvm::PointerType *OpaquePtrTy;      /// %swift.opaque*
  llvm::StructType *ObjCClassStructTy; /// %objc_class
  llvm::PointerType *ObjCClassPtrTy;   /// %objc_class*
  llvm::StructType *ObjCSuperStructTy; /// %objc_super
  llvm::PointerType *ObjCSuperPtrTy;   /// %objc_super*
  llvm::StructType *ObjCBlockStructTy; /// %objc_block
  llvm::PointerType *ObjCBlockPtrTy;   /// %objc_block*
  llvm::StructType *ProtocolConformanceRecordTy;
  llvm::PointerType *ProtocolConformanceRecordPtrTy;
  llvm::StructType *NominalTypeDescriptorTy;
  llvm::PointerType *NominalTypeDescriptorPtrTy;
  llvm::StructType *TypeMetadataRecordTy;
  llvm::PointerType *TypeMetadataRecordPtrTy;
  llvm::StructType *FieldDescriptorTy;
  llvm::PointerType *FieldDescriptorPtrTy;
  llvm::PointerType *ErrorPtrTy;       /// %swift.error*
  llvm::StructType *OpenedErrorTripleTy; /// { %swift.opaque*, %swift.type*, i8** }
  llvm::PointerType *OpenedErrorTriplePtrTy; /// { %swift.opaque*, %swift.type*, i8** }*

  unsigned InvariantMetadataID; /// !invariant.load
  unsigned DereferenceableID;   /// !dereferenceable
  llvm::MDNode *InvariantNode;
  
  llvm::CallingConv::ID C_CC;          /// standard C calling convention
  llvm::CallingConv::ID DefaultCC;     /// default calling convention
  llvm::CallingConv::ID RegisterPreservingCC; /// lightweight calling convention

  llvm::FunctionType *getAssociatedTypeMetadataAccessFunctionTy();
  llvm::FunctionType *getAssociatedTypeWitnessTableAccessFunctionTy();
  llvm::StructType *getGenericWitnessTableCacheTy();

  /// Get the bit width of an integer type for the target platform.
  unsigned getBuiltinIntegerWidth(BuiltinIntegerType *t);
  unsigned getBuiltinIntegerWidth(BuiltinIntegerWidth w);
  
  Size getPointerSize() const { return PtrSize; }
  Alignment getPointerAlignment() const {
    // We always use the pointer's width as its swift ABI alignment.
    return Alignment(PtrSize.getValue());
  }
  Alignment getWitnessTableAlignment() const {
    return getPointerAlignment();
  }
  Alignment getTypeMetadataAlignment() const {
    return getPointerAlignment();
  }

  Size::int_type getOffsetInWords(Size offset) {
    assert(offset.isMultipleOf(getPointerSize()));
    return offset / getPointerSize();
  }

  llvm::Type *getReferenceType(ReferenceCounting style);

  static bool isUnownedReferenceAddressOnly(ReferenceCounting style) {
    switch (style) {
    case ReferenceCounting::Native:
      return false;

    case ReferenceCounting::Unknown:
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
      return true;

    case ReferenceCounting::Bridge:
    case ReferenceCounting::Error:
      llvm_unreachable("unowned references to this type are not supported");
    }

    llvm_unreachable("Not a valid ReferenceCounting.");
  }
  
  /// Return the spare bit mask to use for types that comprise heap object
  /// pointers.
  const SpareBitVector &getHeapObjectSpareBits() const;

  const SpareBitVector &getFunctionPointerSpareBits() const;
  const SpareBitVector &getWitnessTablePtrSpareBits() const;

  SpareBitVector getWeakReferenceSpareBits() const;
  Size getWeakReferenceSize() const { return PtrSize; }
  Alignment getWeakReferenceAlignment() const { return getPointerAlignment(); }

  SpareBitVector getUnownedReferenceSpareBits(ReferenceCounting style) const;
  unsigned getUnownedExtraInhabitantCount(ReferenceCounting style);
  APInt getUnownedExtraInhabitantValue(unsigned bits, unsigned index,
                                       ReferenceCounting style);
  APInt getUnownedExtraInhabitantMask(ReferenceCounting style);

  llvm::Type *getFixedBufferTy();
  llvm::Type *getValueWitnessTy(ValueWitness index);

  void unimplemented(SourceLoc, StringRef Message);
  LLVM_ATTRIBUTE_NORETURN
  void fatal_unimplemented(SourceLoc, StringRef Message);
  void error(SourceLoc loc, const Twine &message);

  bool useDllStorage();

private:
  Size PtrSize;
  llvm::Type *FixedBufferTy;          /// [N x i8], where N == 3 * sizeof(void*)

  llvm::Type *ValueWitnessTys[MaxNumValueWitnesses];
  llvm::FunctionType *AssociatedTypeMetadataAccessFunctionTy = nullptr;
  llvm::FunctionType *AssociatedTypeWitnessTableAccessFunctionTy = nullptr;
  llvm::StructType *GenericWitnessTableCacheTy = nullptr;
  
  llvm::DenseMap<llvm::Type *, SpareBitVector> SpareBitsForTypes;
  
//--- Types -----------------------------------------------------------------
public:
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *D);
  SILType getLoweredType(AbstractionPattern orig, Type subst);
  SILType getLoweredType(Type subst);
  const TypeInfo &getTypeInfoForUnlowered(AbstractionPattern orig,
                                          CanType subst);
  const TypeInfo &getTypeInfoForUnlowered(AbstractionPattern orig,
                                          Type subst);
  const TypeInfo &getTypeInfoForUnlowered(Type subst);
  const TypeInfo &getTypeInfoForLowered(CanType T);
  const TypeInfo &getTypeInfo(SILType T);
  const TypeInfo &getWitnessTablePtrTypeInfo();
  const TypeInfo &getTypeMetadataPtrTypeInfo();
  const TypeInfo &getObjCClassPtrTypeInfo();
  const LoadableTypeInfo &getOpaqueStorageTypeInfo(Size size, Alignment align);
  const LoadableTypeInfo &
  getReferenceObjectTypeInfo(ReferenceCounting refcounting);
  const LoadableTypeInfo &getNativeObjectTypeInfo();
  const LoadableTypeInfo &getUnknownObjectTypeInfo();
  const LoadableTypeInfo &getBridgeObjectTypeInfo();
  const LoadableTypeInfo &getRawPointerTypeInfo();
  llvm::Type *getStorageTypeForUnlowered(Type T);
  llvm::Type *getStorageTypeForLowered(CanType T);
  llvm::Type *getStorageType(SILType T);
  llvm::PointerType *getStoragePointerTypeForUnlowered(Type T);
  llvm::PointerType *getStoragePointerTypeForLowered(CanType T);
  llvm::PointerType *getStoragePointerType(SILType T);
  llvm::StructType *createNominalType(CanType type);
  llvm::StructType *createNominalType(ProtocolCompositionType *T);
  void getSchema(SILType T, ExplosionSchema &schema);
  ExplosionSchema getSchema(SILType T);
  unsigned getExplosionSize(SILType T);
  llvm::PointerType *isSingleIndirectValue(SILType T);
  bool isPOD(SILType type, ResilienceExpansion expansion);
  clang::CanQual<clang::Type> getClangType(CanType type);
  clang::CanQual<clang::Type> getClangType(SILType type);
  clang::CanQual<clang::Type> getClangType(SILParameterInfo param);
  
  const clang::ASTContext &getClangASTContext() {
    assert(ClangASTContext &&
           "requesting clang AST context without clang importer!");
    return *ClangASTContext;
  }

  bool isResilient(NominalTypeDecl *decl, ResilienceExpansion expansion);
  ResilienceExpansion getResilienceExpansionForAccess(NominalTypeDecl *decl);
  ResilienceExpansion getResilienceExpansionForLayout(NominalTypeDecl *decl);
  ResilienceExpansion getResilienceExpansionForLayout(SILGlobalVariable *var);

  SpareBitVector getSpareBitsForType(llvm::Type *scalarTy, Size size);
  
private:
  TypeConverter &Types;
  friend class TypeConverter;

  const clang::ASTContext *ClangASTContext;
  ClangTypeConverter *ClangTypes;
  void initClangTypeConverter();
  void destroyClangTypeConverter();

  friend class GenericContextScope;
  
//--- Globals ---------------------------------------------------------------
public:
  std::pair<llvm::GlobalVariable *, llvm::Constant *>
  createStringConstant(StringRef Str, bool willBeRelativelyAddressed = false,
                       StringRef sectionName = "");
  llvm::Constant *getAddrOfGlobalString(StringRef utf8,
                                        bool willBeRelativelyAddressed = false);
  llvm::Constant *getAddrOfGlobalUTF16String(StringRef utf8);
  llvm::Constant *getAddrOfObjCSelectorRef(StringRef selector);
  llvm::Constant *getAddrOfObjCMethodName(StringRef methodName);
  llvm::Constant *getAddrOfObjCProtocolRecord(ProtocolDecl *proto,
                                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfObjCProtocolRef(ProtocolDecl *proto,
                                           ForDefinition_t forDefinition);
  void addUsedGlobal(llvm::GlobalValue *global);
  void addCompilerUsedGlobal(llvm::GlobalValue *global);
  void addObjCClass(llvm::Constant *addr, bool nonlazy);
  void addProtocolConformanceRecord(NormalProtocolConformance *conformance);

  void addLazyFieldTypeAccessor(NominalTypeDecl *type,
                                ArrayRef<FieldTypeInfo> fieldTypes,
                                llvm::Function *fn);
  llvm::Constant *emitProtocolConformances();
  llvm::Constant *emitTypeMetadataRecords();

  llvm::Constant *getOrCreateHelperFunction(StringRef name,
                                            llvm::Type *resultType,
                                            ArrayRef<llvm::Type*> paramTypes,
                        llvm::function_ref<void(IRGenFunction &IGF)> generate);

private:
  llvm::Constant *getAddrOfClangGlobalDecl(clang::GlobalDecl global,
                                           ForDefinition_t forDefinition);

  llvm::DenseMap<LinkEntity, llvm::Constant*> GlobalVars;
  llvm::DenseMap<LinkEntity, llvm::Constant*> GlobalGOTEquivalents;
  llvm::DenseMap<LinkEntity, llvm::Function*> GlobalFuncs;
  llvm::DenseSet<const clang::Decl *> GlobalClangDecls;
  llvm::StringMap<std::pair<llvm::GlobalVariable*, llvm::Constant*>>
    GlobalStrings;
  llvm::StringMap<llvm::Constant*> GlobalUTF16Strings;
  llvm::StringMap<std::pair<llvm::GlobalVariable*, llvm::Constant*>>
    StringsForTypeRef;
  llvm::DenseMap<CanType, llvm::GlobalVariable*> TypeRefs;
  llvm::StringMap<std::pair<llvm::GlobalVariable*, llvm::Constant*>> FieldNames;
  llvm::StringMap<llvm::Constant*> ObjCSelectorRefs;
  llvm::StringMap<llvm::Constant*> ObjCMethodNames;

  /// LLVMUsed - List of global values which are required to be
  /// present in the object file; bitcast to i8*. This is used for
  /// forcing visibility of symbols which may otherwise be optimized
  /// out.
  SmallVector<llvm::WeakVH, 4> LLVMUsed;

  /// LLVMCompilerUsed - List of global values which are required to be
  /// present in the object file; bitcast to i8*. This is used for
  /// forcing visibility of symbols which may otherwise be optimized
  /// out.
  ///
  /// Similar to LLVMUsed, but emitted as llvm.compiler.used.
  SmallVector<llvm::WeakVH, 4> LLVMCompilerUsed;

  /// Metadata nodes for autolinking info.
  ///
  /// This is typed using llvm::Value instead of llvm::MDNode because it
  /// needs to be used to produce another MDNode during finalization.
  SmallVector<llvm::Metadata *, 32> AutolinkEntries;

  /// List of Objective-C classes, bitcast to i8*.
  SmallVector<llvm::WeakVH, 4> ObjCClasses;
  /// List of Objective-C classes that require nonlazy realization, bitcast to
  /// i8*.
  SmallVector<llvm::WeakVH, 4> ObjCNonLazyClasses;
  /// List of Objective-C categories, bitcast to i8*.
  SmallVector<llvm::WeakVH, 4> ObjCCategories;
  /// List of protocol conformances to generate records for.
  SmallVector<NormalProtocolConformance *, 4> ProtocolConformances;
  /// List of nominal types to generate type metadata records for.
  SmallVector<CanType, 4> RuntimeResolvableTypes;
  /// List of ExtensionDecls corresponding to the generated
  /// categories.
  SmallVector<ExtensionDecl*, 4> ObjCCategoryDecls;

  /// Map of Objective-C protocols and protocol references, bitcast to i8*.
  /// The interesting global variables relating to an ObjC protocol.
  struct ObjCProtocolPair {
    /// The global variable that contains the protocol record.
    llvm::WeakVH record;
    /// The global variable that contains the indirect reference to the
    /// protocol record.
    llvm::WeakVH ref;
  };

  llvm::DenseMap<ProtocolDecl*, ObjCProtocolPair> ObjCProtocols;
  llvm::SmallVector<ProtocolDecl*, 4> LazyObjCProtocolDefinitions;

  /// Uniquing key for a fixed type layout record.
  struct FixedLayoutKey {
    unsigned size;
    unsigned numExtraInhabitants;
    unsigned align: 16;
    unsigned pod: 1;
    unsigned bitwiseTakable: 1;
  };
  friend struct ::llvm::DenseMapInfo<swift::irgen::IRGenModule::FixedLayoutKey>;
  llvm::DenseMap<FixedLayoutKey, llvm::Constant *> PrivateFixedLayouts;

  /// A mapping from order numbers to the LLVM functions which we
  /// created for the SIL functions with those orders.
  SuccessorMap<unsigned, llvm::Function*> EmittedFunctionsByOrder;

  ObjCProtocolPair getObjCProtocolGlobalVars(ProtocolDecl *proto);
  void emitLazyObjCProtocolDefinitions();
  void emitLazyObjCProtocolDefinition(ProtocolDecl *proto);

  void emitGlobalLists();
  void emitAutolinkInfo();
  void cleanupClangCodeGenMetadata();

//--- Remote reflection metadata --------------------------------------------
public:
  /// Section names.
  std::string FieldTypeSection;
  std::string BuiltinTypeSection;
  std::string AssociatedTypeSection;
  std::string CaptureDescriptorSection;
  std::string ReflectionStringsSection;
  std::string ReflectionTypeRefSection;

  /// Builtin types referenced by types in this module when emitting
  /// reflection metadata.
  llvm::SetVector<CanType> BuiltinTypes;
  /// Opaque but fixed-size types for which we also emit builtin type
  /// descriptors, allowing the reflection library to layout these types
  /// without knowledge of their contents. This includes imported structs
  /// and fixed-size multi-payload enums.
  llvm::SetVector<const NominalTypeDecl *> OpaqueTypes;
  /// Imported classes referenced by types in this module when emitting
  /// reflection metadata.
  llvm::SetVector<const ClassDecl *> ImportedClasses;
  /// Imported protocols referenced by types in this module when emitting
  /// reflection metadata.
  llvm::SetVector<const ProtocolDecl *> ImportedProtocols;

  llvm::Constant *getAddrOfStringForTypeRef(StringRef Str);
  llvm::Constant *getAddrOfFieldName(StringRef Name);
  llvm::Constant *getAddrOfCaptureDescriptor(SILFunction &caller,
                                             CanSILFunctionType origCalleeType,
                                             CanSILFunctionType substCalleeType,
                                             SubstitutionList subs,
                                             const HeapLayout &layout);
  llvm::Constant *getAddrOfBoxDescriptor(CanType boxedType);

  void emitAssociatedTypeMetadataRecord(const ProtocolConformance *Conformance);
  void emitFieldMetadataRecord(const NominalTypeDecl *Decl);

  /// Emit a reflection metadata record for a builtin type referenced
  /// from this module.
  void emitBuiltinTypeMetadataRecord(CanType builtinType);

  /// Emit a reflection metadata record for an imported type referenced
  /// from this module.
  void emitOpaqueTypeMetadataRecord(const NominalTypeDecl *nominalDecl);

  /// Some nominal type declarations require us to emit a fixed-size type
  /// descriptor, because they have special layout considerations.
  bool shouldEmitOpaqueTypeMetadataRecord(const NominalTypeDecl *nominalDecl);

  /// Emit reflection metadata records for builtin and imported types referenced
  /// from this module.
  void emitBuiltinReflectionMetadata();

  /// Emit a symbol identifying the reflection metadata version.
  void emitReflectionMetadataVersion();

  const char *getBuiltinTypeMetadataSectionName();
  const char *getFieldTypeMetadataSectionName();
  const char *getAssociatedTypeMetadataSectionName();
  const char *getCaptureDescriptorMetadataSectionName();
  const char *getReflectionStringsSectionName();
  const char *getReflectionTypeRefSectionName();

//--- Runtime ---------------------------------------------------------------
public:
  llvm::Constant *getEmptyTupleMetadata();
  llvm::Constant *getObjCEmptyCachePtr();
  llvm::Constant *getObjCEmptyVTablePtr();
  llvm::Value *getObjCRetainAutoreleasedReturnValueMarker();
  ClassDecl *getObjCRuntimeBaseForSwiftRootClass(ClassDecl *theClass);
  ClassDecl *getObjCRuntimeBaseClass(Identifier name, Identifier objcName);
  llvm::Module *getModule() const;
  llvm::Module *releaseModule();
  llvm::AttributeSet getAllocAttrs();

private:
  llvm::Constant *EmptyTupleMetadata = nullptr;
  llvm::Constant *ObjCEmptyCachePtr = nullptr;
  llvm::Constant *ObjCEmptyVTablePtr = nullptr;
  llvm::Constant *ObjCISAMaskPtr = nullptr;
  Optional<llvm::Value*> ObjCRetainAutoreleasedReturnValueMarker;
  llvm::DenseMap<Identifier, ClassDecl*> SwiftRootClasses;
  llvm::AttributeSet AllocAttrs;

#define FUNCTION_ID(Id)             \
public:                             \
  llvm::Constant *get##Id##Fn();    \
private:                            \
  llvm::Constant *Id##Fn = nullptr;
#include "swift/Runtime/RuntimeFunctions.def"
  
  llvm::Constant *FixLifetimeFn = nullptr;

  mutable Optional<SpareBitVector> HeapPointerSpareBits;
  
//--- Generic ---------------------------------------------------------------
public:
  llvm::Constant *getFixLifetimeFn();
  
  /// The constructor.
  ///
  /// The \p SF is the source file for which the llvm module is generated when
  /// doing multi-threaded whole-module compilation. Otherwise it is null.
  IRGenModule(IRGenerator &irgen,
              std::unique_ptr<llvm::TargetMachine> &&target,
              SourceFile *SF, llvm::LLVMContext &LLVMContext,
              StringRef ModuleName,
              StringRef OutputFilename);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitSourceFile(SourceFile &SF, unsigned StartElem);
  void addLinkLibrary(const LinkLibrary &linkLib);

  /// Attempt to finalize the module.
  ///
  /// This can fail, in which it will return false and the module will be
  /// invalid.
  bool finalize();

  llvm::AttributeSet constructInitialAttributes();

  void emitProtocolDecl(ProtocolDecl *D);
  void emitEnumDecl(EnumDecl *D);
  void emitStructDecl(StructDecl *D);
  void emitClassDecl(ClassDecl *D);
  void emitExtension(ExtensionDecl *D);
  Address emitSILGlobalVariable(SILGlobalVariable *gv);
  void emitCoverageMapping();
  void emitSILFunction(SILFunction *f);
  void emitSILWitnessTable(SILWitnessTable *wt);
  void emitSILStaticInitializers();
  llvm::Constant *emitFixedTypeLayout(CanType t, const FixedTypeInfo &ti);

  void emitNestedTypeDecls(DeclRange members);
  void emitClangDecl(const clang::Decl *decl);
  void finalizeClangCodeGen();
  void finishEmitAfterTopLevel();

  llvm::FunctionType *getFunctionType(CanSILFunctionType type,
                                      llvm::AttributeSet &attrs,
                                      ForeignFunctionInfo *foreignInfo=nullptr);
  ForeignFunctionInfo getForeignFunctionInfo(CanSILFunctionType type);

  llvm::Constant *getSize(Size size);

  Address getAddrOfFieldOffset(VarDecl *D, bool isIndirect,
                               ForDefinition_t forDefinition);
  Address getAddrOfWitnessTableOffset(SILDeclRef fn,
                                      ForDefinition_t forDefinition);
  Address getAddrOfWitnessTableOffset(VarDecl *field,
                                      ForDefinition_t forDefinition);
  llvm::Function *getAddrOfValueWitness(CanType concreteType,
                                        ValueWitness index,
                                        ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfValueWitnessTable(CanType concreteType,
                                         llvm::Type *definitionType = nullptr);
  Optional<llvm::Function*> getAddrOfIVarInitDestroy(ClassDecl *cd,
                                                     bool isDestroyer,
                                                     bool isForeign,
                                                     ForDefinition_t forDefinition);
  llvm::GlobalValue *defineTypeMetadata(CanType concreteType,
                                  bool isIndirect,
                                  bool isPattern,
                                  bool isConstant,
                                  llvm::Constant *init,
                                  std::unique_ptr<llvm::GlobalVariable> replace,
                                  llvm::StringRef section = {});

  llvm::Constant *getAddrOfTypeMetadata(CanType concreteType, bool isPattern);
  ConstantReference getAddrOfTypeMetadata(CanType concreteType, bool isPattern,
                                          SymbolReferenceKind kind);
  llvm::Function *getAddrOfTypeMetadataAccessFunction(CanType type,
                                               ForDefinition_t forDefinition);
  llvm::Function *getAddrOfGenericTypeMetadataAccessFunction(
                                             NominalTypeDecl *nominal,
                                             ArrayRef<llvm::Type *> genericArgs,
                                             ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfTypeMetadataLazyCacheVariable(CanType type,
                                               ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfForeignTypeMetadataCandidate(CanType concreteType);
  llvm::Constant *getAddrOfNominalTypeDescriptor(NominalTypeDecl *D,
                                        llvm::Type *definitionType);
  llvm::Constant *getAddrOfProtocolDescriptor(ProtocolDecl *D,
                                              ForDefinition_t forDefinition,
                                              llvm::Type *definitionType);
  llvm::Constant *getAddrOfObjCClass(ClassDecl *D,
                                     ForDefinition_t forDefinition);
  Address getAddrOfObjCClassRef(ClassDecl *D);
  llvm::Constant *getAddrOfObjCMetaclass(ClassDecl *D,
                                         ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfSwiftMetaclassStub(ClassDecl *D,
                                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfMetaclassObject(ClassDecl *D,
                                           ForDefinition_t forDefinition);
  llvm::Function *getAddrOfSILFunction(SILFunction *f,
                                       ForDefinition_t forDefinition);
  Address getAddrOfSILGlobalVariable(SILGlobalVariable *var,
                                     const TypeInfo &ti,
                                     ForDefinition_t forDefinition);
  llvm::Function *getAddrOfWitnessTableAccessFunction(
                                           const NormalProtocolConformance *C,
                                               ForDefinition_t forDefinition);
  llvm::Function *getAddrOfWitnessTableLazyAccessFunction(
                                           const NormalProtocolConformance *C,
                                               CanType conformingType,
                                               ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfWitnessTableLazyCacheVariable(
                                           const NormalProtocolConformance *C,
                                               CanType conformingType,
                                               ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfWitnessTable(const NormalProtocolConformance *C,
                                        llvm::Type *definitionTy = nullptr);
  llvm::Constant *
  getAddrOfGenericWitnessTableCache(const NormalProtocolConformance *C,
                                    ForDefinition_t forDefinition);
  llvm::Function *
  getAddrOfGenericWitnessTableInstantiationFunction(
                                    const NormalProtocolConformance *C);
  llvm::Function *getAddrOfAssociatedTypeMetadataAccessFunction(
                                           const NormalProtocolConformance *C,
                                           AssociatedTypeDecl *associatedType);
  llvm::Function *getAddrOfAssociatedTypeWitnessTableAccessFunction(
                                           const NormalProtocolConformance *C,
                                           AssociatedTypeDecl *associatedType,
                                           ProtocolDecl *requiredProtocol);

  Address getAddrOfObjCISAMask();

  StringRef mangleType(CanType type, SmallVectorImpl<char> &buffer);
 
  /// Retrieve the generic environment for the current generic context.
  ///
  /// Fails if there is no generic context.
  GenericEnvironment *getGenericEnvironment();

  ConstantReference
  getAddrOfLLVMVariableOrGOTEquivalent(LinkEntity entity, Alignment alignment,
                                       llvm::Type *defaultType);

  llvm::Constant *
  emitRelativeReference(ConstantReference target,
                        llvm::Constant *base,
                        ArrayRef<unsigned> baseIndices);

  llvm::Constant *
  emitDirectRelativeReference(llvm::Constant *target,
                              llvm::Constant *base,
                              ArrayRef<unsigned> baseIndices);

  /// Mark a global variable as true-const by putting it in the text section of
  /// the binary.
  void setTrueConstGlobal(llvm::GlobalVariable *var);

private:
  llvm::Constant *getAddrOfLLVMVariable(LinkEntity entity,
                                        Alignment alignment,
                                        llvm::Type *definitionType,
                                        llvm::Type *defaultType,
                                        DebugTypeInfo debugType);
  llvm::Constant *getAddrOfLLVMVariable(LinkEntity entity,
                                        Alignment alignment,
                                        ForDefinition_t forDefinition,
                                        llvm::Type *defaultType,
                                        DebugTypeInfo debugType);
  ConstantReference getAddrOfLLVMVariable(LinkEntity entity,
                                        Alignment alignment,
                                        llvm::Type *definitionType,
                                        llvm::Type *defaultType,
                                        DebugTypeInfo debugType,
                                        SymbolReferenceKind refKind);

  void checkEligibleConf(const ProtocolConformance *Conf);
  void checkEligibleMetaType(NominalTypeDecl *NT);

  void emitLazyPrivateDefinitions();
  void addRuntimeResolvableType(CanType type);

//--- Global context emission --------------------------------------------------
public:
  void emitRuntimeRegistration();
  void emitVTableStubs();
  void emitTypeVerifier();
private:
  void emitGlobalDecl(Decl *D);
};

/// Stores a pointer to an IRGenModule.
/// As long as the CurrentIGMPtr is alive, the CurrentIGM in the dispatcher
/// is set to the containing IRGenModule.
class CurrentIGMPtr {
  IRGenModule *IGM;

public:
  CurrentIGMPtr(IRGenModule *IGM) : IGM(IGM) {
    assert(IGM);
    assert(!IGM->IRGen.CurrentIGM && "Another CurrentIGMPtr is alive");
    IGM->IRGen.CurrentIGM = IGM;
  }

  ~CurrentIGMPtr() {
    IGM->IRGen.CurrentIGM = nullptr;
  }
  
  IRGenModule *get() const { return IGM; }
  IRGenModule *operator->() const { return IGM; }
};

} // end namespace irgen
} // end namespace swift

namespace llvm {

template<>
struct DenseMapInfo<swift::irgen::IRGenModule::FixedLayoutKey> {
  using FixedLayoutKey = swift::irgen::IRGenModule::FixedLayoutKey;

  static inline FixedLayoutKey getEmptyKey() {
    return {0, 0xFFFFFFFFu, 0, 0, 0};
  }

  static inline FixedLayoutKey getTombstoneKey() {
    return {0, 0xFFFFFFFEu, 0, 0, 0};
  }

  static unsigned getHashValue(const FixedLayoutKey &key) {
    return hash_combine(key.size, key.numExtraInhabitants, key.align,
                        (bool)key.pod, (bool)key.bitwiseTakable);
  }
  static bool isEqual(const FixedLayoutKey &a, const FixedLayoutKey &b) {
    return a.size == b.size
      && a.numExtraInhabitants == b.numExtraInhabitants
      && a.align == b.align
      && a.pod == b.pod
      && a.bitwiseTakable == b.bitwiseTakable;
  }
};

}

#endif
