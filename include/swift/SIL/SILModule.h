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
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/SILLayout.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILCoverageMap.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/Notifications.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/SILPrintContext.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/ilist.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>

namespace swift {
  class AnyFunctionType;
  class ASTContext;
  class FuncDecl;
  class SILUndef;
  class SourceFile;
  class SerializedSILLoader;

  namespace Lowering {
    class SILGenModule;
  }

/// \brief A stage of SIL processing.
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
public:
  using FunctionListType = llvm::ilist<SILFunction>;
  using GlobalListType = llvm::ilist<SILGlobalVariable>;
  using VTableListType = llvm::ilist<SILVTable>;
  using WitnessTableListType = llvm::ilist<SILWitnessTable>;
  using DefaultWitnessTableListType = llvm::ilist<SILDefaultWitnessTable>;
  using CoverageMapListType = llvm::ilist<SILCoverageMap>;
  using LinkingMode = SILOptions::LinkingMode;

private:
  friend class SILBasicBlock;
  friend class SILCoverageMap;
  friend class SILDefaultWitnessTable;
  friend class SILFunction;
  friend class SILGlobalVariable;
  friend class SILLayout;
  friend class SILType;
  friend class SILVTable;
  friend class SILUndef;
  friend class SILWitnessTable;
  friend class Lowering::SILGenModule;
  friend class Lowering::TypeConverter;
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

  /// The last function which was handled in linkTransparentFunctions().
  SILFunction *LastFunctionChecked = nullptr;

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
  llvm::DenseMap<std::pair<const SILVTable *, SILDeclRef>, SILFunction *>
      VTableEntryCache;

  /// Lookup table for SIL witness tables from conformances.
  llvm::DenseMap<const NormalProtocolConformance *, SILWitnessTable *>
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

  // The list of SILCoverageMaps in the module.
  CoverageMapListType coverageMaps;

  /// This is a cache of intrinsic Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, IntrinsicInfo> IntrinsicIDCache;

  /// This is a cache of builtin Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, BuiltinInfo> BuiltinIDCache;

  /// This is the set of undef values we've created, for uniquing purposes.
  llvm::DenseMap<SILType, SILUndef *> UndefValues;

  /// The stage of processing this module is at.
  SILStage Stage;

  /// The callback used by the SILLoader.
  std::unique_ptr<SerializationCallback> Callback;

  /// The SILLoader used when linking functions into this module.
  ///
  /// This is lazily initialized the first time we attempt to
  /// deserialize. Previously this was created when the SILModule was
  /// constructed. In certain cases this was before all Modules had been loaded
  /// causing us to not
  std::unique_ptr<SerializedSILLoader> SILLoader;
  
  /// True if this SILModule really contains the whole module, i.e.
  /// optimizations can assume that they see the whole module.
  bool wholeModule;

  /// The options passed into this SILModule.
  SILOptions &Options;

  /// A list of clients that need to be notified when an instruction
  /// invalidation message is sent.
  llvm::SetVector<DeleteNotificationHandler*> NotificationHandlers;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(ModuleDecl *M, SILOptions &Options, const DeclContext *associatedDC,
            bool wholeModule);

  SILModule(const SILModule&) = delete;
  void operator=(const SILModule&) = delete;

  /// Method which returns the SerializedSILLoader, creating the loader if it
  /// has not been created yet.
  SerializedSILLoader *getSILLoader();

public:
  ~SILModule();

  /// Add a delete notification handler \p Handler to the module context.
  void registerDeleteNotificationHandler(DeleteNotificationHandler* Handler);

  /// Remove the delete notification handler \p Handler from the module context.
  void removeDeleteNotificationHandler(DeleteNotificationHandler* Handler);

  /// Send the invalidation message that \p V is being deleted to all
  /// registered handlers. The order of handlers is deterministic but arbitrary.
  void notifyDeleteHandlers(ValueBase *V);

  /// \brief This converts Swift types to SILTypes.
  mutable Lowering::TypeConverter Types;

  /// Look up the TypeLowering for a SILType.
  const Lowering::TypeLowering &getTypeLowering(SILType t) {
    return Types.getTypeLowering(t);
  }

  /// Invalidate cached entries in SIL Loader.
  void invalidateSILLoaderCaches();

  /// De-serializes all transparent functions for which there is only a
  /// declaration yet (meaning: the body was not de-serialized yet).
  ///
  /// See also: SILFunction::Transparent.
  void linkTransparentFunctions();

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
  /// source file, starting from the specified element number.
  ///
  /// If \p makeModuleFragile is true, all functions and global variables of
  /// the module are marked as fragile. This is used for compiling the stdlib.
  static std::unique_ptr<SILModule>
  constructSIL(ModuleDecl *M, SILOptions &Options, FileUnit *sf = nullptr,
               Optional<unsigned> startElem = None,
               bool makeModuleFragile = false,
               bool isWholeModule = false);

  /// \brief Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static std::unique_ptr<SILModule> createEmptyModule(ModuleDecl *M,
                                                      SILOptions &Options,
                                                      bool WholeModule = false) {
    return std::unique_ptr<SILModule>(new SILModule(M, Options, M, WholeModule));
  }

  /// Get the Swift module associated with this SIL module.
  ModuleDecl *getSwiftModule() const { return TheSwiftModule; }
  /// Get the AST context used for type uniquing etc. by this SIL module.
  ASTContext &getASTContext() const { return TheSwiftModule->getASTContext(); }
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

  using coverage_map_iterator = CoverageMapListType::iterator;
  using coverage_map_const_iterator = CoverageMapListType::const_iterator;
  CoverageMapListType &getCoverageMapList() { return coverageMaps; }
  const CoverageMapListType &getCoverageMapList() const { return coverageMaps; }
  coverage_map_iterator coverage_map_begin() { return coverageMaps.begin(); }
  coverage_map_iterator coverage_map_end() { return coverageMaps.end(); }
  coverage_map_const_iterator coverage_map_begin() const {
    return coverageMaps.begin();
  }
  coverage_map_const_iterator coverage_map_end() const {
    return coverageMaps.end();
  }
  iterator_range<coverage_map_iterator> getCoverageMaps() {
    return {coverageMaps.begin(), coverageMaps.end()};
  }
  iterator_range<coverage_map_const_iterator> getCoverageMaps() const {
    return {coverageMaps.begin(), coverageMaps.end()};
 }

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

  /// Attempt to link the SILFunction. Returns true if linking succeeded, false
  /// otherwise.
  ///
  /// \return false if the linking failed.
  bool linkFunction(SILFunction *Fun,
                    LinkingMode LinkAll = LinkingMode::LinkNormal);

  /// Attempt to link a function by mangled name. Returns true if linking
  /// succeeded, false otherwise.
  ///
  /// \return false if the linking failed.
  bool linkFunction(StringRef Name,
                    LinkingMode LinkAll = LinkingMode::LinkNormal);

  /// Check if a given function exists in any of the modules with a
  /// required linkage, i.e. it can be linked by linkFunction.
  ///
  /// \return null if this module has no such function. Otherwise
  /// the declaration of a function.
  SILFunction *findFunction(StringRef Name, SILLinkage Linkage);

  /// Check if a given function exists in any of the modules.
  /// i.e. it can be linked by linkFunction.
  bool hasFunction(StringRef Name);

  /// Link in all Witness Tables in the module.
  void linkAllWitnessTables();

  /// Link in all VTables in the module.
  void linkAllVTables();

  /// \brief Return the declaration of a utility function that can,
  /// but needn't, be shared between modules.
  SILFunction *getOrCreateSharedFunction(SILLocation loc,
                                         StringRef name,
                                         CanSILFunctionType type,
                                         IsBare_t isBareSILFunction,
                                         IsTransparent_t isTransparent,
                                         IsFragile_t isFragile,
                                         IsThunk_t isThunk);

  /// \brief Return the declaration of a function, or create it if it doesn't
  /// exist.
  SILFunction *getOrCreateFunction(SILLocation loc,
                                   StringRef name,
                                   SILLinkage linkage,
                                   CanSILFunctionType type,
                                   IsBare_t isBareSILFunction,
                                   IsTransparent_t isTransparent,
                                   IsFragile_t isFragile,
                                   IsThunk_t isThunk = IsNotThunk,
                                   SILFunction::ClassVisibility_t CV =
                                           SILFunction::NotRelevant);

  /// \brief Return the declaration of a function, or create it if it doesn't
  /// exist.
  SILFunction *getOrCreateFunction(SILLocation loc,
                                   SILDeclRef constant,
                                   ForDefinition_t forDefinition);

  /// \brief Create a function declaration.
  ///
  /// This signature is a direct copy of the signature of SILFunction::create()
  /// in order to simplify refactoring all SILFunction creation use-sites to use
  /// SILModule. Eventually the uses should probably be refactored.
  SILFunction *createFunction(
      SILLinkage linkage, StringRef name, CanSILFunctionType loweredType,
      GenericEnvironment *genericEnv, Optional<SILLocation> loc,
      IsBare_t isBareSILFunction, IsTransparent_t isTrans,
      IsFragile_t isFragile, IsThunk_t isThunk = IsNotThunk,
      SILFunction::ClassVisibility_t classVisibility = SILFunction::NotRelevant,
      Inline_t inlineStrategy = InlineDefault,
      EffectsKind EK = EffectsKind::Unspecified,
      SILFunction *InsertBefore = nullptr,
      const SILDebugScope *DebugScope = nullptr);

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
    return ! getOptions().AssumeSingleThreaded;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, const SILModule &M){
  M.print(OS);
  return OS;
}

namespace Lowering {
  /// Determine whether the given class will be allocated/deallocated
  /// using the Objective-C runtime, i.e., +alloc and -dealloc.
  LLVM_LIBRARY_VISIBILITY bool usesObjCAllocator(ClassDecl *theClass);
}

} // end swift namespace

#endif
