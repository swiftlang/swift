//===--- IRGenModule.h - Swift Global IR Generation Module ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ClusteredBitVector.h"
#include "swift/Basic/SuccessorMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/CallingConv.h"
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
  class Type;
  namespace CodeGen {
    class CodeGenABITypes;
  }
}
using clang::CodeGen::CodeGenABITypes;

namespace swift {
  class ArchetypeBuilder;
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

namespace irgen {
  class Address;
  class ClangTypeConverter;
  class EnumImplStrategy;
  class ExplosionSchema;
  class FormalType;
  class IRGenDebugInfo;
  class LinkEntity;
  class LoadableTypeInfo;
  class ProtocolInfo;
  class TypeConverter;
  class TypeInfo;
  enum class ValueWitness : unsigned;

class IRGenModule;
  
/// Dispatches IR generation to a single or multiple IRGenModules.
///
/// In single-threaded compilation IRGenModuleDispatcher contains a single
/// IRGenModule. In multi-threaded compilation it contains multiple
/// IRGenModules - one for each LLVM module (= one for each input/output file).
class IRGenModuleDispatcher {
  
public:
  IRGenModuleDispatcher() :
    QueueIndex(0)
  {}

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
  IRGenModule *getGenModule(DeclContext *ctxt) {
    if (GenModules.size() == 1 || !ctxt) {
      return getPrimaryIGM();
    }
    SourceFile *SF = ctxt->getParentSourceFile();
    if (!SF)
      return getPrimaryIGM();
    IRGenModule *IGM = GenModules[SF];
    assert(IGM);
    return IGM;
  }

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
  
  /// Emit everthing which is reachable from already emitted IR.
  void emitLazyDefinitions();
  
  void addLazyFunction(SILFunction *f) {
    // Add it to the queue if it hasn't already been put there.
    if (LazilyEmittedFunctions.insert(f).second)
      LazyFunctionDefinitions.push_back(f);
  }
  
  void addLazyTypeMetadata(CanType type) {
    // Add it to the queue if it hasn't already been put there.
    if (LazilyEmittedTypeMetadata.insert(type).second)
      LazyTypeMetadata.push_back(type);
  }
  
  void addLazyFieldTypeAccessor(NominalTypeDecl *type,
                                NominalTypeDecl::StoredPropertyRange storedProperties,
                                llvm::Function *fn,
                                IRGenModule *IGM) {
    LazyFieldTypeAccessors.push_back({type, storedProperties, fn, IGM});
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

private:
  llvm::DenseMap<SourceFile *, IRGenModule *> GenModules;
  
  IRGenModule *PrimaryIGM = nullptr;
  
  /// The set of type metadata that have been enqueue for lazy emission.
  llvm::SmallPtrSet<CanType, 4> LazilyEmittedTypeMetadata;
  
  /// The queue of lazy type metadata to emit.
  llvm::SmallVector<CanType, 4> LazyTypeMetadata;
  
  llvm::SmallPtrSet<SILFunction*, 4> LazilyEmittedFunctions;

  struct LazyFieldTypeAccessor {
    NominalTypeDecl *type;
    NominalTypeDecl::StoredPropertyRange storedProperties;
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
};

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenModule {
public:
  ASTContext &Context;
  IRGenOptions &Opts;
  std::unique_ptr<clang::CodeGenerator> ClangCodeGen;
  llvm::Module &Module;
  llvm::LLVMContext &LLVMContext;
  const llvm::DataLayout &DataLayout;
  const llvm::Triple &Triple;
  llvm::TargetMachine *TargetMachine;
  SILModule *SILMod;
  llvm::SmallString<128> OutputFilename;
  IRGenModuleDispatcher *dispatcher;
  
  /// Order dependency -- TargetInfo must be initialized after Opts.
  const SwiftTargetInfo TargetInfo;
  /// Holds lexical scope info, etc. Is a nullptr if we compile without -g.
  IRGenDebugInfo *DebugInfo;
  /// A Clang-to-IR-type converter for types appearing in function
  /// signatures of Objective-C methods and C functions.
  CodeGenABITypes *ABITypes;

  /// Does the current target require Objective-C interoperation?
  bool ObjCInterop = true;

  llvm::Type *VoidTy;                  /// void (usually {})
  llvm::IntegerType *Int1Ty;           /// i1
  llvm::IntegerType *Int8Ty;           /// i8
  llvm::IntegerType *Int16Ty;          /// i16
  llvm::IntegerType *Int32Ty;          /// i32
  llvm::IntegerType *Int64Ty;          /// i64
  union {
    llvm::IntegerType *SizeTy;         /// usually i32 or i64
    llvm::IntegerType *IntPtrTy;
    llvm::IntegerType *MetadataKindTy;
    llvm::IntegerType *OnceTy;
  };
  llvm::IntegerType *ObjCBoolTy;       /// i8 or i1
  union {
    llvm::PointerType *Int8PtrTy;      /// i8*
    llvm::PointerType *WitnessTableTy;
    llvm::PointerType *ObjCSELTy;
    llvm::PointerType *FunctionPtrTy;
  };
  union {
    llvm::PointerType *Int8PtrPtrTy;   /// i8**
    llvm::PointerType *WitnessTablePtrTy;
  };
  llvm::StructType *RefCountedStructTy;/// %swift.refcounted = type { ... }
  llvm::PointerType *RefCountedPtrTy;  /// %swift.refcounted*
  llvm::PointerType *WeakReferencePtrTy;/// %swift.weak_reference*
  llvm::Constant *RefCountedNull;      /// %swift.refcounted* null
  llvm::StructType *FunctionPairTy;    /// { i8*, %swift.refcounted* }
  llvm::FunctionType *DeallocatingDtorTy; /// void (%swift.refcounted*)
  llvm::StructType *TypeMetadataStructTy; /// %swift.type = type { ... }
  llvm::PointerType *TypeMetadataPtrTy;/// %swift.type*
  llvm::PointerType *TupleTypeMetadataPtrTy; /// %swift.tuple_type*
  llvm::StructType *FullHeapMetadataStructTy; /// %swift.full_heapmetadata = type { ... }
  llvm::PointerType *FullHeapMetadataPtrTy;/// %swift.full_heapmetadata*
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
  llvm::PointerType *ErrorPtrTy;       /// %swift.error*
  llvm::StructType *OpenedErrorTripleTy; /// { %swift.opaque*, %swift.type*, i8** }
  llvm::PointerType *OpenedErrorTriplePtrTy; /// { %swift.opaque*, %swift.type*, i8** }*
  
  llvm::CallingConv::ID RuntimeCC;     /// lightweight calling convention

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
  
  /// Return the spare bit mask to use for types that comprise heap object
  /// pointers.
  const SpareBitVector &getHeapObjectSpareBits() const;

  const SpareBitVector &getFunctionPointerSpareBits() const;
  SpareBitVector getWeakReferenceSpareBits() const;
  const SpareBitVector &getWitnessTablePtrSpareBits() const;

  Size getWeakReferenceSize() const { return PtrSize; }
  Alignment getWeakReferenceAlignment() const { return getPointerAlignment(); }

  llvm::Type *getFixedBufferTy();
  llvm::Type *getValueWitnessTy(ValueWitness index);

  void unimplemented(SourceLoc, StringRef Message);
  LLVM_ATTRIBUTE_NORETURN
  void fatal_unimplemented(SourceLoc, StringRef Message);
  void error(SourceLoc loc, const Twine &message);

private:
  Size PtrSize;
  llvm::Type *FixedBufferTy;          /// [N x i8], where N == 3 * sizeof(void*)

  llvm::Type *ValueWitnessTys[MaxNumValueWitnesses];
  
  llvm::DenseMap<llvm::Type *, SpareBitVector> SpareBitsForTypes;
  
  std::unique_ptr<const EnumImplStrategy> TheUnimplementedEnumImplStrategy;

//--- Types -----------------------------------------------------------------
public:
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *D);
  SILType getLoweredType(AbstractionPattern orig, Type subst);
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
  const LoadableTypeInfo &getUnknownObjectTypeInfo();
  const LoadableTypeInfo &getBridgeObjectTypeInfo();
  llvm::Type *getStorageTypeForUnlowered(Type T);
  llvm::Type *getStorageTypeForLowered(CanType T);
  llvm::Type *getStorageType(SILType T);
  llvm::PointerType *getStoragePointerTypeForUnlowered(Type T);
  llvm::PointerType *getStoragePointerTypeForLowered(CanType T);
  llvm::PointerType *getStoragePointerType(SILType T);
  llvm::StructType *createNominalType(TypeDecl *D);
  llvm::StructType *createNominalType(ProtocolCompositionType *T);
  void getSchema(SILType T, ExplosionSchema &schema);
  ExplosionSchema getSchema(SILType T);
  unsigned getExplosionSize(SILType T);
  llvm::PointerType *isSingleIndirectValue(SILType T);
  llvm::PointerType *requiresIndirectResult(SILType T);
  bool isTrivialMetatype(CanMetatypeType type);
  bool isPOD(SILType type, ResilienceScope scope);
  ObjectSize classifyTypeSize(SILType type, ResilienceScope scope);
  clang::CanQual<clang::Type> getClangType(CanType type);
  clang::CanQual<clang::Type> getClangType(SILType type);
  
  const EnumImplStrategy &getUnimplementedEnumImplStrategy();

  const clang::ASTContext &getClangASTContext() {
    assert(ClangASTContext &&
           "requesting clang AST context without clang importer!");
    return *ClangASTContext;
  }

  bool isResilient(Decl *decl, ResilienceScope scope);

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
  llvm::Constant *getAddrOfGlobalString(StringRef utf8);
  llvm::Constant *getAddrOfGlobalUTF16String(StringRef utf8);
  llvm::Constant *getAddrOfObjCSelectorRef(StringRef selector);
  llvm::Constant *getAddrOfObjCMethodName(StringRef methodName);
  llvm::Constant *getAddrOfObjCProtocolRecord(ProtocolDecl *proto,
                                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfObjCProtocolRef(ProtocolDecl *proto,
                                           ForDefinition_t forDefinition);
  void addUsedGlobal(llvm::GlobalValue *global);
  void addObjCClass(llvm::Constant *addr, bool nonlazy);
  void addProtocolConformanceRecord(llvm::Constant *record);

  void addLazyFieldTypeAccessor(NominalTypeDecl *type,
                          NominalTypeDecl::StoredPropertyRange storedProperties,
                          llvm::Function *fn);

private:
  llvm::DenseMap<LinkEntity, llvm::Constant*> GlobalVars;
  llvm::DenseMap<LinkEntity, llvm::Function*> GlobalFuncs;
  llvm::DenseSet<const clang::Decl *> GlobalClangDecls;
  llvm::StringMap<llvm::Constant*> GlobalStrings;
  llvm::StringMap<llvm::Constant*> GlobalUTF16Strings;
  llvm::StringMap<llvm::Constant*> ObjCSelectorRefs;
  llvm::StringMap<llvm::Constant*> ObjCMethodNames;

  /// LLVMUsed - List of global values which are required to be
  /// present in the object file; bitcast to i8*. This is used for
  /// forcing visibility of symbols which may otherwise be optimized
  /// out.
  SmallVector<llvm::WeakVH, 4> LLVMUsed;

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
  /// List of protocol conformance records.
  SmallVector<llvm::WeakVH, 4> ProtocolConformanceRecords;
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
  /// A mapping from order numbers to the LLVM functions which we
  /// created for the SIL functions with those orders.
  SuccessorMap<unsigned, llvm::Function*> EmittedFunctionsByOrder;

  ObjCProtocolPair getObjCProtocolGlobalVars(ProtocolDecl *proto);

  llvm::Constant *emitProtocolConformances();
  void emitGlobalLists();
  void emitAutolinkInfo();

//--- Runtime ---------------------------------------------------------------
public:
  llvm::Constant *getEmptyTupleMetadata();
  llvm::Constant *getObjCEmptyCachePtr();
  llvm::Constant *getObjCEmptyVTablePtr();
  llvm::Value *getObjCRetainAutoreleasedReturnValueMarker();
  ClassDecl *getSwiftRootClass();
  llvm::Module *getModule() const;
  llvm::Module *releaseModule();
  llvm::AttributeSet getAllocAttrs();

private:
  llvm::Constant *EmptyTupleMetadata = nullptr;
  llvm::Constant *ObjCEmptyCachePtr = nullptr;
  llvm::Constant *ObjCEmptyVTablePtr = nullptr;
  llvm::Constant *ObjCISAMaskPtr = nullptr;
  Optional<llvm::Value*> ObjCRetainAutoreleasedReturnValueMarker;
  ClassDecl *SwiftRootClass = nullptr;
  llvm::AttributeSet AllocAttrs;  

#define FUNCTION_ID(Id)             \
public:                             \
  llvm::Constant *get##Id##Fn();    \
private:                            \
  llvm::Constant *Id##Fn = nullptr;
#include "RuntimeFunctions.def"

  mutable Optional<SpareBitVector> HeapPointerSpareBits;
  
//--- Generic ---------------------------------------------------------------
public:
  IRGenModule(IRGenModuleDispatcher *dispatcher, SourceFile *SF,
              ASTContext &Context,
              llvm::LLVMContext &LLVMContext,
              IRGenOptions &Opts, StringRef ModuleName,
              const llvm::DataLayout &DataLayout,
              const llvm::Triple &Triple,
              llvm::TargetMachine *TargetMachine,
              SILModule *SILMod,
              StringRef OutputFilename);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitSourceFile(SourceFile &SF, unsigned StartElem);
  void addLinkLibrary(const LinkLibrary &linkLib);
  void finalize();

  void emitProtocolDecl(ProtocolDecl *D);
  void emitEnumDecl(EnumDecl *D);
  void emitStructDecl(StructDecl *D);
  void emitClassDecl(ClassDecl *D);
  void emitExtension(ExtensionDecl *D);
  Address emitSILGlobalVariable(SILGlobalVariable *gv);
  void emitCoverageMapping();
  void emitSILFunction(SILFunction *f);
  void emitSILWitnessTable(SILWitnessTable *wt);
  void emitSILStaticInitializer();
  
  void emitNestedTypeDecls(DeclRange members);
  void emitClangDecl(clang::Decl *decl);
  void finishEmitAfterTopLevel();

  llvm::FunctionType *getFunctionType(CanSILFunctionType type,
                                      llvm::AttributeSet &attrs);

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
  Optional<llvm::Function*> getAddrOfObjCIVarInitDestroy(
                              ClassDecl *cd,
                              bool isDestroyer,
                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfTypeMetadata(CanType concreteType,
                                        bool isIndirect, bool isPattern,
                                        llvm::Type *definitionType = nullptr);
  llvm::Function *getAddrOfTypeMetadataAccessFunction(CanType type,
                                               ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfTypeMetadataLazyCacheVariable(CanType type,
                                               ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfForeignTypeMetadataCandidate(CanType concreteType);
  llvm::Constant *getAddrOfNominalTypeDescriptor(NominalTypeDecl *D,
                                        llvm::Type *definitionType);
  llvm::Constant *getAddrOfProtocolDescriptor(ProtocolDecl *D,
                                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfObjCClass(ClassDecl *D,
                                     ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfObjCMetaclass(ClassDecl *D,
                                         ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfSwiftMetaclassStub(ClassDecl *D,
                                              ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfMetaclassObject(ClassDecl *D,
                                           ForDefinition_t forDefinition);
  llvm::Function *getAddrOfSILFunction(SILFunction *f,
                                       ForDefinition_t forDefinition);
  Address getAddrOfSILGlobalVariable(SILGlobalVariable *var,
                                     ForDefinition_t forDefinition);
  llvm::Constant *getAddrOfWitnessTable(const NormalProtocolConformance *C,
                                        llvm::Type *definitionTy = nullptr);
  Address getAddrOfObjCISAMask();

  StringRef mangleType(CanType type, SmallVectorImpl<char> &buffer);
  
  // Get the ArchetypeBuilder for the currently active generic context. Crashes
  // if there is no generic context.
  ArchetypeBuilder &getContextArchetypes();

//--- Global context emission --------------------------------------------------
public:
  void emitRuntimeRegistration();
  void emitVTableStubs();
  void emitTypeVerifier();
private:
  void emitGlobalDecl(Decl *D);
  void emitExternalDefinition(Decl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
