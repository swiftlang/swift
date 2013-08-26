//===--- IRGenDebugInfo.h - Debug Info Support-------------------*- C++ -*-===//
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
// This file defines IR codegen support for debug information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DEBUGINFO_H
#define SWIFT_IRGEN_DEBUGINFO_H

#include "llvm/Support/ValueHandle.h"
#include "llvm/DebugInfo.h"
#include "llvm/DIBuilder.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Dwarf.h"

#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/AST/Stmt.h"

#include "DebugTypeInfo.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

#include <set>

namespace llvm {
  class DIBuilder;
}

namespace clang {
  class TargetInfo;
}

namespace swift {

class ASTContext;
class SILArgument;
class SILDebugScope;
class SILModule;
class SILFunctionTypeInfo;
class AllocStackInst;

namespace irgen {

class Options;

//===--- BEGIN "TO BE FIXED IN A SWIFT BRANCH OF LLVM" ---===//
//
// DWARF constants.
//
// The first unused language value in DWARF v5 is DW_LANG_Haskell+1 =
// 0x19.  We can't use it, because LLVM asserts that there are no
// languages >DW_LANG_Python=0x14.  Wouldn't it would be much more
// appropriate to use a constant in DW_LANG_lo_user..DW_LANG_hi_user
// anyway, you may ask? Well, CompileUnit::constructTypeDIE() will
// always use a DW_FORM_data1, which is too small for that range!  And
// by fixing that in LLVM we would hint at developing a new language.
// So instead, let's hijack a language with a very low potential for
// accidental conflicts for now.
typedef enum {
  DW_LANG_Swift = 0xf,
  DW_LANG_ObjC = llvm::dwarf::DW_LANG_ObjC, // For symmetry.
  // Reuse some existing tag so the verifier doesn't complain.
  DW_TAG_meta_type = llvm::dwarf::DW_TAG_restrict_type
} dwarf;
//
//===--------- END "TO BE FIXED IN A SWIFT BRANCH OF LLVM" --------===//

typedef struct {
  unsigned Line, Col;
  const char* Filename;
} Location;

/// IRGenDebugInfo - Helper object that keeps track of the current
/// CompileUnit, File, LexicalScope, and translates SILLocations into
/// <llvm::DebugLoc>s.
class IRGenDebugInfo {
  const Options &Opts;
  const clang::TargetInfo &TargetInfo;
  SourceManager &SM;
  llvm::DIBuilder DBuilder;
  TypeConverter &Types;

  // Various caches.
  llvm::DenseMap<SILDebugScope *, llvm::WeakVH> ScopeCache;
  llvm::DenseMap<const char *, llvm::WeakVH> DIFileCache;
  llvm::DenseMap<TypeBase*, llvm::WeakVH> DITypeCache;
  std::map<std::string, llvm::WeakVH> DINameSpaceCache;

  // Subprograms need their scope to be RAUW'd when we work through
  // the list of imports.
  std::map<StringRef, llvm::WeakVH> Functions;

  // These are used by getArgNo.
  SILFunction *LastFn;
  SILBasicBlock::const_bbarg_iterator LastArg, LastEnd;
  unsigned LastArgNo;

  StringRef MainFilename;
  StringRef CWDName; /// The current working directory.
  llvm::BumpPtrAllocator DebugInfoNames;
  llvm::DICompileUnit TheCU;
  llvm::DIFile MainFile;

  Location LastLoc; /// The last location that was emitted.
  SILDebugScope *LastScope; /// The scope of that last location.

  SmallVector<std::pair<Location, SILDebugScope*>, 8> LocationStack;

public:
  IRGenDebugInfo(const Options &Opts,
                 const clang::TargetInfo &TargetInfo,
                 TypeConverter &Types,
                 SourceManager &SM,
                 llvm::Module &M);

  /// Finalize the llvm::DIBuilder owned by this object.
  void finalize();

  /// Update the IRBuilder's current debug location to the location
  /// Loc and the lexical scope DS.
  void setCurrentLoc(IRBuilder &Builder, SILDebugScope *DS,
                     SILLocation Loc = SILLocation());

  /// Push the current debug location onto a stack.
  void pushLoc() {
    LocationStack.push_back(std::make_pair(LastLoc, LastScope));
  }

  /// Restore the current debug location from the stack.
  void popLoc() {
    std::tie(LastLoc, LastScope) = LocationStack.pop_back_val();
  }

  /// Emit debug info for an import declaration.
  void emitImport(ImportDecl *D);

  /// Emit debug info for the given function.
  /// \param DS The parent scope of the function.
  /// \param Fn The IR representation of the function.
  /// \param CC The calling convention of the function.
  /// \param Ty The signature of the function.
  void emitFunction(SILModule &SILMod, SILDebugScope *DS, llvm::Function *Fn,
                    AbstractCC CC, SILType Ty);

  /// Emit debug info for a given SIL function.
  void emitFunction(SILFunction *SILFn, llvm::Function *Fn);


  /// Convenience function useful for functions without any source
  /// location. Internally calls emitFunction, emits a debug
  /// scope, and finally sets it using setCurrentLoc.
  inline void emitArtificialFunction(IRGenFunction &IGF, llvm::Function *Fn) {
    emitArtificialFunction(*IGF.IGM.SILMod, IGF.Builder, Fn);
  }
  void emitArtificialFunction(SILModule &SILMod, IRBuilder &Builder,
                                llvm::Function *Fn);

  /// Emit a dbg.declare instrinsic at the current insertion point and
  /// the Builder's current debug location.
  /// \param Tag The DWARF tag that should be used.
  void emitVariableDeclaration(IRBuilder& Builder,
                               llvm::Value *Storage,
                               DebugTypeInfo Ty,
                               StringRef Name,
                               unsigned Tag,
                               unsigned ArgNo = 0);

  /// Convenience function for stack-allocated variables. Calls
  /// emitVariableDeclaration internally.
  void emitStackVariableDeclaration(IRBuilder& Builder,
                                    llvm::Value *Storage,
                                    DebugTypeInfo Ty,
                                    StringRef Name,
                                    swift::AllocStackInst *i);

  /// Convenience function for variables that are function arguments.
  void emitArgVariableDeclaration(IRBuilder& Builder,
                                  llvm::Value *Storage,
                                  DebugTypeInfo Ty,
                                  StringRef Name,
                                  unsigned ArgNo);

  /// Emit debug metadata for a global variable.
  void emitGlobalVariableDeclaration(llvm::GlobalValue *Storage,
                                     StringRef Name,
                                     StringRef LinkageName,
                                     DebugTypeInfo DebugType,
                                     SILLocation Loc);

  StringRef getMainFilename() const { return MainFilename; }

private:
  llvm::DIType createType(DebugTypeInfo DbgTy, llvm::DIDescriptor Scope,
                          llvm::DIFile File);
  llvm::DIType getOrCreateType(DebugTypeInfo DbgTy, llvm::DIDescriptor Scope);
  llvm::DIDescriptor getOrCreateScope(SILDebugScope *DS);
  StringRef getCurrentDirname();
  llvm::DIFile getOrCreateFile(const char *Filename);
  StringRef getName(const FuncDecl& FD);
  StringRef getName(SILLocation L);
  StringRef getMangledName(CanType CanTy);
  StringRef getMangledName(StringRef MangledName);
  llvm::DIArray createParameterTypes(SILModule &SILMod,
                                     SILType SILTy,
                                     llvm::FunctionType *IRTy,
                                     llvm::DIDescriptor Scope);
  llvm::DIArray getTupleElements(TupleType *TupleTy, llvm::DIDescriptor Scope,
                                 llvm::DIFile File, unsigned Flags);
  unsigned getArgNo(SILFunction *Fn, SILArgument *Arg);
  llvm::DIFile getFile(llvm::DIDescriptor Scope);
  llvm::DIScope getOrCreateNamespace(llvm::DIScope Namespace,
                                     std::string MangledName,
                                     llvm::DIFile File, unsigned Line = 0);
  llvm::DIScope getNamespace(StringRef MangledName);
  llvm::DIArray getStructMembers(NominalTypeDecl *D, llvm::DIDescriptor Scope,
                                 llvm::DIFile File, unsigned Flags);
  llvm::DICompositeType createStructType(DebugTypeInfo DbgTy,
                                         NominalTypeDecl *Decl,
                                         StringRef Name,
                                         llvm::DIDescriptor Scope,
                                         llvm::DIFile File, unsigned Line,
                                         unsigned SizeInBits,
                                         unsigned AlignInBits,
                                         unsigned Flags,
                                         llvm::DIType DerivedFrom,
                                         unsigned RuntimeLang);
  llvm::DIDerivedType createMemberType(CanType CTy,
                                       unsigned &OffsetInBits,
                                       llvm::DIDescriptor Scope,
                                       llvm::DIFile File,
                                       unsigned Flags);
  llvm::DIArray getUnionElements(UnionDecl *D, llvm::DIDescriptor Scope,
                                 llvm::DIFile File,
                                 unsigned SizeInBytes, unsigned AlignInBytes,
                                 unsigned Flags);
  llvm::DICompositeType
  createUnionType(DebugTypeInfo DbgTy, UnionDecl *Decl, StringRef Name,
                  llvm::DIDescriptor Scope, llvm::DIFile File, unsigned Line,
                  unsigned SizeInBytes, unsigned AlignInBytes, unsigned Flags);
};

} // irgen
} // swift

#endif
