//===--- SILModule.h - Defines the SILModule class --------------*- C++ -*-===//
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
// This file defines the SILModule class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILMODULE_H
#define SWIFT_SIL_SILMODULE_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILCoverageMap.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
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
  class SILExternalSource;
  class SILTypeList;
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
};

/// \brief A SIL module. The SIL module owns all of the SILFunctions generated
/// when a Swift compilation context is lowered to SIL.
class SILModule {
public:
  using FunctionListType = llvm::ilist<SILFunction>;
  using GlobalListType = llvm::ilist<SILGlobalVariable>;
  using VTableListType = llvm::ilist<SILVTable>;
  using WitnessTableListType = llvm::ilist<SILWitnessTable>;
  using CoverageMapListType = llvm::ilist<SILCoverageMap>;
  using LinkingMode = SILOptions::LinkingMode;

private:
  friend class SILBasicBlock;
  friend class SILCoverageMap;
  friend class SILFunction;
  friend class SILGlobalVariable;
  friend class SILType;
  friend class SILVTable;
  friend class SILUndef;
  friend class SILWitnessTable;
  friend class Lowering::SILGenModule;
  friend class Lowering::TypeConverter;
  class SerializationCallback;

  /// Allocator that manages the memory of all the pieces of the SILModule.
  mutable llvm::BumpPtrAllocator BPA;
  void *TypeListUniquing;

  /// The swift Module associated with this SILModule.
  Module *TheSwiftModule;

  /// A specific context for AST-level declarations associated with this SIL
  /// module.
  ///
  /// \sa getAssociatedContext
  const DeclContext *AssociatedDeclContext;

  /// Lookup table for SIL functions. This needs to be declared before \p
  /// functions so that the destructor of \p functions is called first.
  llvm::StringMap<SILFunction *> FunctionTable;

  /// The list of SILFunctions in the module.
  FunctionListType functions;

  /// Functions, which are dead (and not in the functions list anymore),
  /// but kept alive for debug info generation.
  FunctionListType zombieFunctions;
  
  /// Stores the names of zombie functions.
  llvm::BumpPtrAllocator zombieFunctionNames;
  
  /// Lookup table for SIL vtables from class decls.
  llvm::DenseMap<const ClassDecl *, SILVTable *> VTableLookupTable;

  /// The list of SILVTables in the module.
  VTableListType vtables;

  /// Lookup table for SIL witness tables from conformances.
  llvm::DenseMap<const NormalProtocolConformance *, SILWitnessTable *>
  WitnessTableLookupCache;

  /// The list of SILWitnessTables in the module.
  WitnessTableListType witnessTables;

  /// Lookup table for SIL Global Variables.
  llvm::StringMap<SILGlobalVariable *> GlobalVariableTable;

  /// The list of SILGlobalVariables in the module.
  GlobalListType silGlobals;

  // The list of SILCoverageMaps in the module.
  CoverageMapListType coverageMaps;

  /// This is a cache of intrinsic Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, IntrinsicInfo> IntrinsicIDCache;

  /// This is a cache of builtin Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, BuiltinInfo> BuiltinIDCache;

  /// This is the set of undef values we've created, for uniquing purposes.
  llvm::DenseMap<SILType, SILUndef*> UndefValues;

  /// A mapping of SILFunctions -> SILDeclRefs.
  ///
  /// This is populated when ever a SILDeclRef is used to construct a function
  /// via getOrCreateFunction(SILLocation, SILDeclRef, ForDefinition_t). It
  /// currently may have false negatives, i.e., SILFunctions with SILDeclRefs
  /// which are created via the other getOrCreateFunction. Thus this should only
  /// be used for conservative queries. It is currently just being used to
  /// trigger extra verification of functions with self in the SIL verifier.
  llvm::DenseMap<const SILFunction *, SILDeclRef> FunctionToDeclRefMap;

  /// The stage of processing this module is at.
  SILStage Stage;

  /// The callback used by the SILLoader.
  std::unique_ptr<SerializationCallback> Callback;

  /// The SILLoader used when linking functions into this module.
  ///
  /// This is lazily initialized the first time we attempt to
  /// deserialize. Previously this was created when the SILModule was
  /// constructed. In certain cases this was before all Modules had been loaded
  /// causeing us to not
  std::unique_ptr<SerializedSILLoader> SILLoader;
  
  /// True if this SILModule really contains the whole module, i.e.
  /// optimizations can assume that they see the whole module.
  bool wholeModule;

  /// The external SIL source to use when linking this module.
  SILExternalSource *ExternalSource = nullptr;

  /// The options passed into this SILModule.
  SILOptions &Options;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(Module *M, SILOptions &Options, const DeclContext *associatedDC,
            bool wholeModule);

  SILModule(const SILModule&) = delete;
  void operator=(const SILModule&) = delete;

  /// Method which returns the SerializedSILLoader, creating the loader if it
  /// has not been created yet.
  SerializedSILLoader *getSILLoader();

public:
  ~SILModule();

  /// \brief Get a uniqued pointer to a SIL type list.
  SILTypeList *getSILTypeList(ArrayRef<SILType> Types) const;

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
  constructSIL(Module *M, SILOptions &Options, FileUnit *sf = nullptr,
               Optional<unsigned> startElem = None,
               bool makeModuleFragile = false,
               bool isWholeModule = false);

  /// \brief Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static std::unique_ptr<SILModule> createEmptyModule(Module *M,
                                                      SILOptions &Options) {
    return std::unique_ptr<SILModule>(new SILModule(M, Options, M, false));
  }

  /// Get the Swift module associated with this SIL module.
  Module *getSwiftModule() const { return TheSwiftModule; }
  /// Get the AST context used for type uniquing etc. by this SIL module.
  ASTContext &getASTContext() const { return TheSwiftModule->Ctx; }
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
  Range<iterator> getFunctions() {
    return {functions.begin(), functions.end()};
  }
  Range<const_iterator> getFunctions() const {
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
  Range<vtable_iterator> getVTables() {
    return {vtables.begin(), vtables.end()};
  }
  Range<vtable_const_iterator> getVTables() const {
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
  Range<witness_table_iterator> getWitnessTables() {
    return {witnessTables.begin(), witnessTables.end()};
  }
  Range<witness_table_const_iterator> getWitnessTables() const {
    return {witnessTables.begin(), witnessTables.end()};
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
  Range<sil_global_iterator> getSILGlobals() {
    return {silGlobals.begin(), silGlobals.end()};
  }
  Range<sil_global_const_iterator> getSILGlobals() const {
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
  Range<coverage_map_iterator> getCoverageMaps() {
    return {coverageMaps.begin(), coverageMaps.end()};
  }
  Range<coverage_map_const_iterator> getCoverageMaps() const {
    return {coverageMaps.begin(), coverageMaps.end()};
 }

  /// Look for a global variable by name.
  ///
  /// \return null if this module has no such global variable
 SILGlobalVariable *lookUpGlobalVariable(StringRef name) const {
    return GlobalVariableTable.lookup(name);
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

  /// Look for a declaration for a function.
  ///
  /// Returns an optional since some functions will not have SILDeclRefs.
  llvm::Optional<SILDeclRef> lookUpDeclRef(const SILFunction *F) const;

  /// Attempt to link the SILFunction. Returns true if linking succeeded, false
  /// otherwise.
  ///
  /// \return false if the linking failed.
  bool linkFunction(SILFunction *Fun,
                    LinkingMode LinkAll=LinkingMode::LinkNormal,
                    std::function<void(SILFunction *)> Callback =nullptr);

  /// Attempt to link a function by declaration. Returns true if linking
  /// succeeded, false otherwise.
  ///
  /// \return false if the linking failed.
  bool linkFunction(SILDeclRef Decl,
                    LinkingMode LinkAll=LinkingMode::LinkNormal,
                    std::function<void(SILFunction *)> Callback =nullptr);

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
  /// exist..
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
  /// exist..
  SILFunction *getOrCreateFunction(SILLocation loc,
                                   SILDeclRef constant,
                                   ForDefinition_t forDefinition);

  /// Look up the SILWitnessTable representing the lowering of a protocol
  /// conformance, and collect the substitutions to apply to the referenced
  /// witnesses, if any.
  ///
  /// \arg C The protocol conformance mapped key to use to lookup the witness
  ///        table.
  /// \arg deserializeLazily If we can not find the witness table should we
  ///                        attempt to lazily deserialize it.
  std::pair<SILWitnessTable *, ArrayRef<Substitution>>
  lookUpWitnessTable(const ProtocolConformance *C, bool deserializeLazily=true);

  /// Attempt to lookup \p Member in the witness table for C.
  std::tuple<SILFunction *, SILWitnessTable *, ArrayRef<Substitution>>
  lookUpFunctionInWitnessTable(const ProtocolConformance *C, SILDeclRef Member);

  /// Look up the VTable mapped to the given ClassDecl. Returns null on failure.
  SILVTable *lookUpVTable(const ClassDecl *C);

  /// Attempt to lookup the function corresponding to \p Member in the class
  /// hierarchy of \p Class.
  SILFunction *lookUpFunctionInVTable(ClassDecl *Class, SILDeclRef Member);

  // Given a protocol conformance, attempt to create a witness table declaration
  // for it.
  SILWitnessTable *
  createWitnessTableDeclaration(ProtocolConformance *C, SILLinkage linkage);

  /// \brief Return the stage of processing this module is at.
  SILStage getStage() const { return Stage; }

  /// \brief Advance the module to a further stage of processing.
  void setStage(SILStage s) {
    assert(s >= Stage && "regressing stage?!");
    Stage = s;
  }

  SILExternalSource *getExternalSource() const { return ExternalSource; }
  void setExternalSource(SILExternalSource *S) {
    assert(!ExternalSource && "External source already set");
    ExternalSource = S;
  }

  /// \brief Run the SIL verifier to make sure that all Functions follow
  /// invariants.
  void verify() const;

  /// Pretty-print the module.
  void dump() const;

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
             Module *M = nullptr, bool ShouldSort = false,
             bool PrintASTDecls = true) const;

  /// Allocate memory using the module's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const {
    if (getASTContext().LangOpts.UseMalloc)
      return AlignedAlloc(Size, Align);

    return BPA.Allocate(Size, Align);
  }

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
