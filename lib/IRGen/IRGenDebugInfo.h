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
#include "llvm/Support/SourceMgr.h"

#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/AST/Stmt.h"

#include "DebugTypeInfo.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

namespace llvm {
  class DIBuilder;
}

namespace swift {

class SILArgument;
class SILDebugScope;
class SILModule;
class SILFunctionTypeInfo;
class AllocStackInst;

namespace irgen {

class Options;

typedef struct {
  unsigned Line, Col;
  const char* Filename;
} Location;

/// IRGenDebugInfo - Helper object that keeps track of the current
/// CompileUnit, File, LexicalScope, and translates SILLocations into
/// <llvm::DebugLoc>s.
class IRGenDebugInfo {
  llvm::SourceMgr &SM;
  llvm::DIBuilder DBuilder;
  const Options &Opts;
  TypeConverter &Types;

  // Various caches.
  llvm::DenseMap<SILDebugScope *, llvm::DIDescriptor> ScopeCache;
  llvm::DenseMap<const char *, llvm::WeakVH> DIFileCache;
  llvm::DenseMap<DebugTypeInfo, llvm::WeakVH> DITypeCache;

  // These are used by getArgNo.
  SILFunction *LastFn;
  SILBasicBlock::const_bbarg_iterator LastArg, LastEnd;
  unsigned LastArgNo;

  StringRef CWDName; /// The current working directory.
  llvm::BumpPtrAllocator DebugInfoNames;
  llvm::DICompileUnit TheCU;
  llvm::DIFile MainFile;

  Location LastLoc; /// The last location that was emitted.
  SILDebugScope *LastScope; /// The scope of that last location.

  llvm::SmallVector<std::pair<Location, SILDebugScope*>, 8> LocationStack;

public:
  IRGenDebugInfo(const Options &Opts, TypeConverter &Types,
                 llvm::SourceMgr &SM, llvm::Module &M);

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

  /// Create debug info for the given function.
  /// \param DS The parent scope of the function.
  /// \param Fn The IR representation of the function.
  /// \param CC The calling convention of the function.
  /// \param Ty The signature of the function.
  void createFunction(SILModule &SILMod, SILDebugScope *DS, llvm::Function *Fn,
                      AbstractCC CC, SILType Ty);

  /// Create debug info for a given SIL function.
  void createFunction(SILFunction *SILFn, llvm::Function *Fn);


  /// Convenience function useful for functions without any source
  /// location. Internally calls createFunction, creates a debug
  /// scope, and finally sets it using setCurrentLoc.
  inline void createArtificialFunction(IRGenFunction &IGF, llvm::Function *Fn) {
    createArtificialFunction(*IGF.IGM.SILMod, IGF.Builder, Fn);
  }
  void createArtificialFunction(SILModule &SILMod, IRBuilder &Builder,
                                llvm::Function *Fn);

  /// Emit a dbg.declare instrinsic at the current insertion point and
  /// the Builder's current debug location.
  /// \param Tag The DWARF tag that should be used.
  void emitVariableDeclaration(IRBuilder& Builder,
                               llvm::Value *Storage,
                               DebugTypeInfo Ty,
                               const llvm::Twine &Name,
                               unsigned Tag,
                               unsigned ArgNo = 0);

  /// Convenience function for stack-allocated variables. Calls
  /// emitVariableDeclaration internally.
  void emitStackVariableDeclaration(IRBuilder& Builder,
                                    llvm::Value *Storage,
                                    DebugTypeInfo Ty,
                                    const llvm::Twine &Name,
                                    swift::AllocStackInst *i);

  /// Convenience function for variables that are function arguments.
  void emitArgVariableDeclaration(IRBuilder& Builder,
                                  llvm::Value *Storage,
                                  DebugTypeInfo Ty,
                                  const llvm::Twine &Name,
                                  unsigned ArgNo);

  /// Emit debug metadata for a global variable.
  void emitGlobalVariableDeclaration(llvm::GlobalValue *Storage,
                                     StringRef Name,
                                     StringRef LinkageName,
                                     DebugTypeInfo DebugType,
                                     SILLocation Loc);

private:
  llvm::DIType createType(DebugTypeInfo Ty, llvm::DIDescriptor Scope,
                          llvm::DIFile File);
  llvm::DIType getOrCreateType(DebugTypeInfo Ty, llvm::DIDescriptor Scope);
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
  llvm::DIArray getTupleElements(TupleType *TupleTy, llvm::DIDescriptor Scope);
  unsigned getArgNo(SILFunction *Fn, SILArgument *Arg);
  llvm::DIFile getFile(llvm::DIDescriptor Scope);
};

} // irgen
} // swift

#endif
