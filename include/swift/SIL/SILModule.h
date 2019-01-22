//===--- SILModule.h - Defines the SILModule class --------------*- C++ -*-===//
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
// This file defines the SILModule class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILMODULE_H
#define SWIFT_SIL_SILMODULE_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/ProfileCounter.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/Notifications.h"
#include "swift/SIL/SILCoverageMap.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILPrintContext.h"
#include "swift/SIL/SILProperty.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ProfileData/InstrProfReader.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>

namespace llvm {
namespace yaml {
class Output;
} // end namespace yaml
} // end namespace llvm

namespace swift {

class AnyFunctionType;
class ASTContext;
class FileUnit;
class FuncDecl;
class KeyPathPattern;
class ModuleDecl;
class SILUndef;
class SourceFile;
class SerializedSILLoader;
class SILFunctionBuilder;

namespace Lowering {
class SILGenModule;
} // namespace Lowering

/// A stage of SIL processing.
enum class SILStage {
  /// \brief "Raw" SIL, emitted by SILGen, but not yet run through guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Raw SIL does not have fully-constructed SSA and may contain undiagnosed
  /// dataflow errors.
  Raw,

  /// \brief Canonical SIL, which has been run through at least the guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Canonical SIL has stricter invariants than raw SIL. It must not contain
  /// dataflow errors, and some instructions must be canonicalized to simpler
  /// forms.
  Canonical,

  /// \brief Lowered SIL, which has been prepared for IRGen and will no longer
  /// be passed to canonical SIL transform passes.
  ///
  /// In lowered SIL, the SILType of all SILValues is its SIL storage
  /// type. Explicit storage is required for all address-only and resilient
  /// types.
  ///
  /// Generating the initial Raw SIL is typically referred to as lowering (from
  /// the AST). To disambiguate, refer to the process of generating the lowered
  /// stage of SIL as "address lowering".
  Lowered,
};

/// \brief A SIL module. The SIL module owns all of the SILFunctions generated
/// when a Swift compilation context is lowered to SIL.
class SILModule {
  friend class SILFunctionBuilder;

public:
  using FunctionListType = llvm::ilist<SILFunction>;
  using GlobalListType = llvm::ilist<SILGlobalVariable>;
  using VTableListType = llvm::ilist<SILVTable>;
  using PropertyListType = llvm::ilist<SILProperty>;
  using WitnessTableListType = llvm::ilist<SILWitnessTable>;
  using DefaultWitnessTableListType = llvm::ilist<SILDefaultWitnessTable>;
  using CoverageMapCollectionType =
      llvm::MapVector<StringRef, SILCoverageMap *>;

  enum class LinkingMode : uint8_t {
    /// Link functions with non-public linkage. Used by the mandatory pipeline.
    LinkNormal,

    /// Link all functions. Used by the performance pipeine.
    LinkAll
  };

  using ActionCallback = std::function<void()>;

private:
  friend KeyPathPattern;
  friend SILBasicBlock;
  friend SILCoverageMap;
  friend SILDefaultWitnessTable;
  friend SILFunction;
  friend SILGlobalVariable;
  friend SILLayout;
  friend SILType;
  friend SILVTable;
  friend SILProperty;
  friend SILUndef;
  friend SILWitnessTable;
  friend Lowering::SILGenModule;
  friend Lowering::TypeConverter;
  class SerializationCallback;

  /// Allocator that manages the memory of all the pieces of the SILModule.
  mutable llvm::BumpPtrAllocator BPA;

  /// The swift Module associated with this SILModule.
  ModuleDecl *TheSwiftModule;

  /// A specific context for AST-level declarations associated with this SIL
  /// module.
  ///
  /// \sa getAssociatedContext
  const DeclContext *AssociatedDeclContext;

  /// Lookup table for SIL functions. This needs to be declared before \p
  /// functions so that the destructor of \p functions is called first.
  llvm::StringMap<SILFunction *> FunctionTable;
  llvm::StringMap<SILFunction *> ZombieFunctionTable;

  /// The list of SILFunctions in the module.
  FunctionListType functions;

  /// Functions, which are dead (and not in the functions list anymore),
  /// but kept alive for debug info generation.
  FunctionListType zombieFunctions;
  
  /// Stores the names of zombie functions.
  llvm::BumpPtrAllocator zombieFunctionNames;
  
  /// Lookup table for SIL vtables from class decls.
  llvm::DenseMap<const ClassDecl *, SILVTable *> VTableMap;

  /// The list of SILVTables in the module.
  VTableListType vtables;

  /// This is a cache of vtable entries for quick look-up
  llvm::DenseMap<std::pair<const SILVTable *, SILDeclRef>, SILVTable::Entry>
      VTableEntryCache;

  /// Lookup table for SIL witness tables from conformances.
  llvm::DenseMap<const RootProtocolConformance *, SILWitnessTable *>
  WitnessTableMap;

  /// The list of SILWitnessTables in the module.
  WitnessTableListType witnessTables;

  /// Lookup table for SIL default witness tables from protocols.
  llvm::DenseMap<const ProtocolDecl *, SILDefaultWitnessTable *>
  DefaultWitnessTableMap;

  /// The list of SILDefaultWitnessTables in the module.
  DefaultWitnessTableListType defaultWitnessTables;

  /// Lookup table for SIL Global Variables.
  llvm::StringMap<SILGlobalVariable *> GlobalVariableMap;

  /// The list of SILGlobalVariables in the module.
  GlobalListType silGlobals;

  // The map of SILCoverageMaps in the module.
  CoverageMapCollectionType coverageMaps;
  
  // The list of SILProperties in the module.
  PropertyListType properties;

  /// This is the underlying raw stream of OptRecordStream.
  ///
  /// It is also owned by SILModule in order to keep their lifetime in sync.
  std::unique_ptr<llvm::raw_ostream> OptRecordRawStream;

  /// If non-null, the YAML file where remarks should be recorded.
  std::unique_ptr<llvm::yaml::Output> OptRecordStream;

  /// This is a cache of intrinsic Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, IntrinsicInfo> IntrinsicIDCache;

  /// This is a cache of builtin Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, BuiltinInfo> BuiltinIDCache;

  /// This is the set of undef values we've created, for uniquing purposes.
  llvm::DenseMap<SILType, SILUndef *> UndefValues;

  /// The stage of processing this module is at.
  SILStage Stage;

  /// The set of deserialization notification handlers.
  DeserializationNotificationHandlerSet deserializationNotificationHandlers;

  /// The SILLoader used when linking functions into this module.
  ///
  /// This is lazily initialized the first time we attempt to
  /// deserialize. Previously this was created when the SILModule was
  /// constructed. In certain cases this was before all Modules had been loaded
  /// causing us to not
  std::unique_ptr<SerializedSILLoader> SILLoader;

  /// The indexed profile data to be used for PGO, or nullptr.
  std::unique_ptr<llvm::IndexedInstrProfReader> PGOReader;

  /// True if this SILModule really contains the whole module, i.e.
  /// optimizations can assume that they see the whole module.
  bool wholeModule;

  /// The options passed into this SILModule.
  SILOptions &Options;

  /// Set if the SILModule was serialized already. It is used
  /// to ensure that the module is serialized only once.
  bool serialized;

  /// Action to be executed for serializing the SILModule.
  ActionCallback SerializeSILAction;

  /// A list of clients that need to be notified when an instruction
  /// invalidation message is sent.
  llvm::SetVector<DeleteNotificationHandler*> NotificationHandlers;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(ModuleDecl *M, SILOptions &Options, const DeclContext *associatedDC,
            bool wholeModule);

  SILModule(const SILModule&) = delete;
  void operator=(const SILModule&) = delete;

  // SWIFT_ENABLE_TENSORFLOW
public:
  /// Method which returns the SerializedSILLoader, creating the loader if it
  /// has not been created yet.
  SerializedSILLoader *getSILLoader();

  /// Folding set for key path patterns.
  llvm::FoldingSet<KeyPathPattern> KeyPathPatterns;

public:
  ~SILModule();

  /// Add a callback for each newly deserialized SIL function body.
  void registerDeserializationNotificationHandler(
      std::unique_ptr<DeserializationNotificationHandler> &&handler);

  /// Return the set of registered deserialization callbacks.
  DeserializationNotificationHandlerSet::range
  getDeserializationHandlers() const {
    return deserializationNotificationHandlers.getRange();
  }

  void removeDeserializationNotificationHandler(
      DeserializationNotificationHandler *handler) {
    deserializationNotificationHandlers.erase(handler);
  }

  /// Add a delete notification handler \p Handler to the module context.
  void registerDeleteNotificationHandler(DeleteNotificationHandler* Handler);

  /// Remove the delete notification handler \p Handler from the module context.
  void removeDeleteNotificationHandler(DeleteNotificationHandler* Handler);

  /// Send the invalidation message that \p V is being deleted to all
  /// registered handlers. The order of handlers is deterministic but arbitrary.
  void notifyDeleteHandlers(SILNode *node);

  /// Set a serialization action.
  void setSerializeSILAction(ActionCallback SerializeSILAction);
  ActionCallback getSerializeSILAction() const;

  /// Set a flag indicating that this module is serialized already.
  void setSerialized() { serialized = true; }
  bool isSerialized() const { return serialized; }

  /// Serialize a SIL module using the configured SerializeSILAction.
  void serialize();

  /// \brief This converts Swift types to SILTypes.
  mutable Lowering::TypeConverter Types;

  /// Look up the TypeLowering for a SILType.
  const Lowering::TypeLowering &getTypeLowering(SILType t) {
    return Types.getTypeLowering(t);
  }

  /// Invalidate cached entries in SIL Loader.
  void invalidateSILLoaderCaches();

  /// Erase a function from the module.
  void eraseFunction(SILFunction *F);

  /// Invalidate a function in SILLoader cache.
  void invalidateFunctionInSILCache(SILFunction *F);

  /// Specialization can cause a function that was erased before by dead function
  /// elimination to become alive again. If this happens we need to remove it
  /// from the list of zombies.
  void removeFromZombieList(StringRef Name);

  /// Erase a global SIL variable from the module.
  void eraseGlobalVariable(SILGlobalVariable *G);

  /// Construct a SIL module from an AST module.
  ///
  /// The module will be constructed in the Raw stage. The provided AST module
  /// should contain source files.
  ///
  /// If a source file is provided, SIL will only be emitted for decls in that
  /// source file.
  static std::unique_ptr<SILModule>
  constructSIL(ModuleDecl *M, SILOptions &Options, FileUnit *sf = nullptr);

  /// \brief Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static std::unique_ptr<SILModule>
  createEmptyModule(ModuleDecl *M, SILOptions &Options,
                    bool WholeModule = false);

  /// Get the Swift module associated with this SIL module.
  ModuleDecl *getSwiftModule() const { return TheSwiftModule; }
  /// Get the AST context used for type uniquing etc. by this SIL module.
  ASTContext &getASTContext() const;
  SourceManager &getSourceManager() const { return getASTContext().SourceMgr; }

  /// Get the Swift DeclContext associated with this SIL module.
  ///
  /// All AST declarations within this context are assumed to have been fully
  /// processed as part of generating this module. This allows certain passes
  /// to make additional assumptions about these declarations.
  ///
  /// If this is the same as TheSwiftModule, the entire module is being
  /// compiled as a single unit. If this is null, no context-based assumptions
  /// can be made.
  const DeclContext *getAssociatedContext() const {
    return AssociatedDeclContext;
  }

  /// Returns true if this SILModule really contains the whole module, i.e.
  /// optimizations can assume that they see the whole module.
  bool isWholeModule() const {
    return wholeModule;
  }

  bool isStdlibModule() const;

  /// Returns true if it is the optimized OnoneSupport module.
  bool isOptimizedOnoneSupportModule() const;

  SILOptions &getOptions() const { return Options; }

  using iterator = FunctionListType::iterator;
  using const_iterator = FunctionListType::const_iterator;
  FunctionListType &getFunctionList() { return functions; }
  const FunctionListType &getFunctionList() const { return functions; }
  iterator begin() { return functions.begin(); }
  iterator end() { return functions.end(); }
  const_iterator begin() const { return functions.begin(); }
  const_iterator end() const { return functions.end(); }
  iterator_range<iterator> getFunctions() {
    return {functions.begin(), functions.end()};
  }
  iterator_range<const_iterator> getFunctions() const {
    return {functions.begin(), functions.end()};
  }

  const_iterator zombies_begin() const { return zombieFunctions.begin(); }
  const_iterator zombies_end() const { return zombieFunctions.end(); }

  using vtable_iterator = VTableListType::iterator;
  using vtable_const_iterator = VTableListType::const_iterator;
  VTableListType &getVTableList() { return vtables; }
  const VTableListType &getVTableList() const { return vtables; }
  vtable_iterator vtable_begin() { return vtables.begin(); }
  vtable_iterator vtable_end() { return vtables.end(); }
  vtable_const_iterator vtable_begin() const { return vtables.begin(); }
  vtable_const_iterator vtable_end() const { return vtables.end(); }
  iterator_range<vtable_iterator> getVTables() {
    return {vtables.begin(), vtables.end()};
  }
  iterator_range<vtable_const_iterator> getVTables() const {
    return {vtables.begin(), vtables.end()};
  }

  using witness_table_iterator = WitnessTableListType::iterator;
  using witness_table_const_iterator = WitnessTableListType::const_iterator;
  WitnessTableListType &getWitnessTableList() { return witnessTables; }
  const WitnessTableListType &getWitnessTableList() const { return witnessTables; }
  witness_table_iterator witness_table_begin() { return witnessTables.begin(); }
  witness_table_iterator witness_table_end() { return witnessTables.end(); }
  witness_table_const_iterator witness_table_begin() const { return witnessTables.begin(); }
  witness_table_const_iterator witness_table_end() const { return witnessTables.end(); }
  iterator_range<witness_table_iterator> getWitnessTables() {
    return {witnessTables.begin(), witnessTables.end()};
  }
  iterator_range<witness_table_const_iterator> getWitnessTables() const {
    return {witnessTables.begin(), witnessTables.end()};
  }

  using default_witness_table_iterator = DefaultWitnessTableListType::iterator;
  using default_witness_table_const_iterator = DefaultWitnessTableListType::const_iterator;
  DefaultWitnessTableListType &getDefaultWitnessTableList() { return defaultWitnessTables; }
  const DefaultWitnessTableListType &getDefaultWitnessTableList() const { return defaultWitnessTables; }
  default_witness_table_iterator default_witness_table_begin() { return defaultWitnessTables.begin(); }
  default_witness_table_iterator default_witness_table_end() { return defaultWitnessTables.end(); }
  default_witness_table_const_iterator default_witness_table_begin() const { return defaultWitnessTables.begin(); }
  default_witness_table_const_iterator default_witness_table_end() const { return defaultWitnessTables.end(); }
  iterator_range<default_witness_table_iterator> getDefaultWitnessTables() {
    return {defaultWitnessTables.begin(), defaultWitnessTables.end()};
  }
  iterator_range<default_witness_table_const_iterator> getDefaultWitnessTables() const {
    return {defaultWitnessTables.begin(), defaultWitnessTables.end()};
  }

  using sil_global_iterator = GlobalListType::iterator;
  using sil_global_const_iterator = GlobalListType::const_iterator;
  GlobalListType &getSILGlobalList() { return silGlobals; }
  const GlobalListType &getSILGlobalList() const { return silGlobals; }
  sil_global_iterator sil_global_begin() { return silGlobals.begin(); }
  sil_global_iterator sil_global_end() { return silGlobals.end(); }
  sil_global_const_iterator sil_global_begin() const {
    return silGlobals.begin();
  }
  sil_global_const_iterator sil_global_end() const {
    return silGlobals.end();
  }
  iterator_range<sil_global_iterator> getSILGlobals() {
    return {silGlobals.begin(), silGlobals.end()};
  }
  iterator_range<sil_global_const_iterator> getSILGlobals() const {
    return {silGlobals.begin(), silGlobals.end()};
  }

  using coverage_map_iterator = CoverageMapCollectionType::iterator;
  using coverage_map_const_iterator = CoverageMapCollectionType::const_iterator;
  CoverageMapCollectionType &getCoverageMaps() { return coverageMaps; }
  const CoverageMapCollectionType &getCoverageMaps() const {
    return coverageMaps;
  }

  llvm::yaml::Output *getOptRecordStream() { return OptRecordStream.get(); }
  void setOptRecordStream(std::unique_ptr<llvm::yaml::Output> &&Stream,
                          std::unique_ptr<llvm::raw_ostream> &&RawStream);

  // This is currently limited to VarDecl because the visibility of global
  // variables and class properties is straightforward, while the visibility of
  // class methods (ValueDecls) depends on the subclass scope. "Visiblity" has
  // a different meaning when vtable layout is at stake.
  bool isVisibleExternally(const VarDecl *decl) {
    return isPossiblyUsedExternally(getDeclSILLinkage(decl), isWholeModule());
  }

  PropertyListType &getPropertyList() { return properties; }
  const PropertyListType &getPropertyList() const { return properties; }
  
  /// Look for a global variable by name.
  ///
  /// \return null if this module has no such global variable
  SILGlobalVariable *lookUpGlobalVariable(StringRef name) const {
    return GlobalVariableMap.lookup(name);
  }

  /// Look for a function by name.
  ///
  /// \return null if this module has no such function
  SILFunction *lookUpFunction(StringRef name) const {
    return FunctionTable.lookup(name);
  }

  /// Look for a function by declaration.
  ///
  /// \return null if this module has no such function
  SILFunction *lookUpFunction(SILDeclRef fnRef);

  /// Attempt to deserialize the SILFunction. Returns true if deserialization
  /// succeeded, false otherwise.
  bool loadFunction(SILFunction *F);

  /// Attempt to link the SILFunction. Returns true if linking succeeded, false
  /// otherwise.
  ///
  /// \return false if the linking failed.
  bool linkFunction(SILFunction *F,
                    LinkingMode LinkMode = LinkingMode::LinkNormal);

  /// Check if a given function exists in any of the modules with a
  /// required linkage, i.e. it can be linked by linkFunction.
  ///
  /// \return null if this module has no such function. Otherwise
  /// the declaration of a function.
  SILFunction *findFunction(StringRef Name, SILLinkage Linkage);

  /// Check if a given function exists in any of the modules.
  /// i.e. it can be linked by linkFunction.
  bool hasFunction(StringRef Name);

  /// Link all definitions in all segments that are logically part of
  /// the same AST module.
  void linkAllFromCurrentModule();

  /// Look up the SILWitnessTable representing the lowering of a protocol
  /// conformance, and collect the substitutions to apply to the referenced
  /// witnesses, if any.
  ///
  /// \arg C The protocol conformance mapped key to use to lookup the witness
  ///        table.
  /// \arg deserializeLazily If we cannot find the witness table should we
  ///                        attempt to lazily deserialize it.
  SILWitnessTable *
  lookUpWitnessTable(ProtocolConformanceRef C, bool deserializeLazily=true);
  SILWitnessTable *
  lookUpWitnessTable(const ProtocolConformance *C, bool deserializeLazily=true);

  /// Attempt to lookup \p Member in the witness table for \p C.
  std::pair<SILFunction *, SILWitnessTable *>
  lookUpFunctionInWitnessTable(ProtocolConformanceRef C,
                               SILDeclRef Requirement);

  /// Look up the SILDefaultWitnessTable representing the default witnesses
  /// of a resilient protocol, if any.
  SILDefaultWitnessTable *lookUpDefaultWitnessTable(const ProtocolDecl *Protocol,
                                                    bool deserializeLazily=true);

  /// Attempt to lookup \p Member in the default witness table for \p Protocol.
  std::pair<SILFunction *, SILDefaultWitnessTable *>
  lookUpFunctionInDefaultWitnessTable(const ProtocolDecl *Protocol,
                                      SILDeclRef Requirement,
                                      bool deserializeLazily=true);

  /// Look up the VTable mapped to the given ClassDecl. Returns null on failure.
  SILVTable *lookUpVTable(const ClassDecl *C);

  /// Attempt to lookup the function corresponding to \p Member in the class
  /// hierarchy of \p Class.
  SILFunction *lookUpFunctionInVTable(ClassDecl *Class, SILDeclRef Member);

  // Given a protocol conformance, attempt to create a witness table declaration
  // for it.
  SILWitnessTable *
  createWitnessTableDeclaration(ProtocolConformance *C, SILLinkage linkage);

  // Given a protocol, attempt to create a default witness table declaration
  // for it.
  SILDefaultWitnessTable *
  createDefaultWitnessTableDeclaration(const ProtocolDecl *Protocol,
                                       SILLinkage Linkage);

  /// Deletes a dead witness table.
  void deleteWitnessTable(SILWitnessTable *Wt);

  /// \brief Return the stage of processing this module is at.
  SILStage getStage() const { return Stage; }

  /// \brief Advance the module to a further stage of processing.
  void setStage(SILStage s) {
    assert(s >= Stage && "regressing stage?!");
    Stage = s;
  }

  llvm::IndexedInstrProfReader *getPGOReader() const { return PGOReader.get(); }

  void setPGOReader(std::unique_ptr<llvm::IndexedInstrProfReader> IPR) {
    PGOReader = std::move(IPR);
  }

  /// Can value operations (copies and destroys) on the given lowered type
  /// be performed in this module?
  bool isTypeABIAccessible(SILType type);

  /// Can type metadata for the given formal type be fetched in
  /// the given module?
  bool isTypeMetadataAccessible(CanType type);

  /// \brief Run the SIL verifier to make sure that all Functions follow
  /// invariants.
  void verify() const;

  /// Pretty-print the module.
  void dump(bool Verbose = false) const;
  
  /// Pretty-print the module to a file.
  /// Useful for dumping the module when running in a debugger.
  /// Warning: no error handling is done. Fails with an assert if the file
  /// cannot be opened.
  void dump(const char *FileName, bool Verbose = false,
            bool PrintASTDecls = false) const;

  /// Pretty-print the module to the designated stream.
  ///
  /// \param Verbose Dump SIL location information in verbose mode.
  /// \param M If present, the types and declarations from this module will be
  ///        printed. The module would usually contain the types and Decls that
  ///        the SIL module depends on.
  /// \param ShouldSort If set to true sorts functions, vtables, sil global
  ///        variables, and witness tables by name to ease diffing.
  /// \param PrintASTDecls If set to true print AST decls.
  void print(raw_ostream &OS, bool Verbose = false,
             ModuleDecl *M = nullptr, bool ShouldSort = false,
             bool PrintASTDecls = true) const {
    SILPrintContext PrintCtx(OS, Verbose, ShouldSort);
    print(PrintCtx, M, PrintASTDecls);
  }

  /// Pretty-print the module with the context \p PrintCtx.
  ///
  /// \param M If present, the types and declarations from this module will be
  ///        printed. The module would usually contain the types and Decls that
  ///        the SIL module depends on.
  /// \param PrintASTDecls If set to true print AST decls.
  void print(SILPrintContext &PrintCtx, ModuleDecl *M = nullptr,
             bool PrintASTDecls = true) const;

  /// Allocate memory using the module's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const;

  template <typename T> T *allocate(unsigned Count) const {
    return static_cast<T *>(allocate(sizeof(T) * Count, alignof(T)));
  }

  template <typename T>
  MutableArrayRef<T> allocateCopy(ArrayRef<T> Array) const {
    MutableArrayRef<T> result(allocate<T>(Array.size()), Array.size());
    std::uninitialized_copy(Array.begin(), Array.end(), result.begin());
    return result;
  }

  StringRef allocateCopy(StringRef Str) const {
    auto result = allocateCopy<char>({Str.data(), Str.size()});
    return {result.data(), result.size()};
  }

  /// Allocate memory for an instruction using the module's internal allocator.
  void *allocateInst(unsigned Size, unsigned Align) const;

  /// Deallocate memory of an instruction.
  void deallocateInst(SILInstruction *I);

  /// \brief Looks up the llvm intrinsic ID and type for the builtin function.
  ///
  /// \returns Returns llvm::Intrinsic::not_intrinsic if the function is not an
  /// intrinsic. The particular intrinsic functions which correspond to the
  /// returned value are defined in llvm/Intrinsics.h.
  const IntrinsicInfo &getIntrinsicInfo(Identifier ID);

  /// \brief Looks up the lazily cached identification for the builtin function.
  ///
  /// \returns Returns builtin info of BuiltinValueKind::None kind if the
  /// declaration is not a builtin.
  const BuiltinInfo &getBuiltinInfo(Identifier ID);

  /// Returns true if the builtin or intrinsic is no-return.
  bool isNoReturnBuiltinOrIntrinsic(Identifier Name);

  /// Returns true if the default atomicity of the module is Atomic.
  bool isDefaultAtomic() const {
    return !getOptions().AssumeSingleThreaded;
  }

  /// Returns true if SIL entities associated with declarations in the given
  /// declaration context ought to be serialized as part of this module.
  bool
  shouldSerializeEntitiesAssociatedWithDeclContext(const DeclContext *DC) const;
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, const SILModule &M){
  M.print(OS);
  return OS;
}

namespace Lowering {
/// Determine whether the given class will be allocated/deallocated using the
/// Objective-C runtime, i.e., +alloc and -dealloc.
LLVM_LIBRARY_VISIBILITY bool usesObjCAllocator(ClassDecl *theClass);
} // namespace Lowering

} // namespace swift

#endif
