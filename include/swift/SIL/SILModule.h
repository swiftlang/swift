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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/ilist.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
  class AnyFunctionType;
  class ASTContext;
  class FuncDecl;
  class SILExternalSource;
  class SILTypeList;
  class SILUndef;
  class SourceFile;
  class TranslationUnit;

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
/// when a Shily module is lowered to SIL.
class SILModule {
public:
  using FunctionListType = llvm::ilist<SILFunction>;
  using GlobalListType = llvm::ilist<SILGlobalVariable>;
  using VTableListType = llvm::ilist<SILVTable>;
  
private:
  friend class SILBasicBlock;
  friend class SILFunction;
  friend class SILGlobalVariable;
  friend class SILType;
  friend class SILVTable;
  friend class SILUndef;
  friend class Lowering::SILGenModule;
  friend class Lowering::TypeConverter;

  /// Allocator that manages the memory of all the pieces of the SILModule.
  mutable llvm::BumpPtrAllocator BPA;
  void *TypeListUniquing;

  /// The swift Module associated with this SILModule.
  Module *TheSwiftModule;
  
  /// The list of SILFunctions in the module.
  FunctionListType functions;
  
  /// The list of SILVTables in the module.
  VTableListType vtables;
  
  /// The list of SILGlobalVariables in the module.
  /// FIXME: Merge with 'globals'.
  GlobalListType silGlobals;
  
  /// The collection of global variables used in the module.
  /// FIXME: Remove this when SILGlobalVariable is ready.
  llvm::SetVector<VarDecl*> globals;

  /// This is a cache of intrinsic Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, IntrinsicInfo> IntrinsicIDCache;

  /// This is a cache of builtin Function declarations to numeric ID mappings.
  llvm::DenseMap<Identifier, BuiltinInfo> BuiltinIDCache;

  /// This is the set of undef values we've created, for uniquing purposes.
  llvm::DenseMap<SILType, SILUndef*> UndefValues;

  /// The stage of processing this module is at.
  SILStage Stage;

  /// The external SIL source to use when linking this module.
  SILExternalSource *ExternalSource = nullptr;
  
  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(Module *M);
  
  SILModule(const SILModule&) = delete;
  void operator=(const SILModule&) = delete;

public:
  ~SILModule();

  /// \brief Get a uniqued pointer to a SIL type list.
  SILTypeList *getSILTypeList(ArrayRef<SILType> Types) const;

  /// \brief This converts Swift types to SILTypes.
  Lowering::TypeConverter Types;
  
  /// Look up the TypeLowering for a SILType.
  const Lowering::TypeLowering &getTypeLowering(SILType t) {
    return Types.getTypeLowering(t);
  }
  
  /// Construct a SIL module from a translation unit. The module will be
  /// constructed in the Raw stage. It is the caller's responsibility to
  /// `delete` the module.
  ///
  /// If a source file is provided, SIL will only be emitted for decls in that
  /// source file, starting from the specified element number.
  static std::unique_ptr<SILModule> constructSIL(TranslationUnit *tu,
                                                 SourceFile *sf = nullptr,
                                                 unsigned startElem = 0);

  /// \brief Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static std::unique_ptr<SILModule> createEmptyModule(Module *M) {
    return std::unique_ptr<SILModule>(new SILModule(M));
  }
  
  /// Get the Swift module associated with this SIL module.
  Module *getSwiftModule() const { return TheSwiftModule; }
  /// Get the AST context used for type uniquing etc. by this SIL module.
  ASTContext &getASTContext() const { return TheSwiftModule->Ctx; }
  
  // FIXME: Remove these when SILGlobalVariable is ready to take over.
  
  using global_iterator = decltype(globals)::const_iterator;
  using GlobalRange = Range<global_iterator>;
  
  /// Returns the set of global variables in this module.
  GlobalRange getGlobals() const {
    return {globals.begin(), globals.end()};
  }
  global_iterator global_begin() const {
    return globals.begin();
  }
  global_iterator global_end() const {
    return globals.end();
  }
  
  using iterator = FunctionListType::iterator;
  using const_iterator = FunctionListType::const_iterator;
  FunctionListType &getFunctionList() { return functions; }
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
  
  using vtable_iterator = VTableListType::iterator;
  using vtable_const_iterator = VTableListType::const_iterator;
  VTableListType &getVTableList() { return vtables; }
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
  
  using sil_global_iterator = GlobalListType::iterator;
  using sil_global_const_iterator = GlobalListType::const_iterator;
  GlobalListType &getSILGlobalList() { return silGlobals; }
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

  SILFunction *lookup(StringRef Name) {
    // FIXME: Linear lookup is ridiculous here.
    for (SILFunction &F : *this)
      if (F.getName() == Name)
        return &F;
    return nullptr;
  }
  
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
  /// \param TU Passing the optional translation unit will result in the types
  ///        and the declarations from the corresponding unit to be printed. The
  ///        TU usually would contain the types and Decls that the SIL Module
  ///        depends on.
  void print(raw_ostream &OS, bool Verbose = false,
             TranslationUnit *TU = nullptr) const;

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


} // end swift namespace

#endif
