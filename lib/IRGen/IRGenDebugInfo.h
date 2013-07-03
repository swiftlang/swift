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
#include "swift/AST/Stmt.h"

#include "DebugTypeInfo.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

namespace llvm {
class DIBuilder;
}

namespace swift {

class SILDebugScope;
class SILModule;

namespace irgen {

class Options;

/// IRGenDebugInfo - Helper object that keeps track of the current
/// CompileUnit, File, LexicalScope, and translates SILLocations into
/// <llvm::DebugLoc>s.
class IRGenDebugInfo {
  llvm::SourceMgr &SM;
  llvm::DIBuilder DBuilder;
  llvm::DenseMap<SILDebugScope*, llvm::DIDescriptor> ScopeCache;
  llvm::DenseMap<const char *, llvm::WeakVH> DIFileCache;
  llvm::DenseMap<DebugTypeInfo, llvm::WeakVH> DITypeCache;
  /// The current working directory.
  StringRef CWDName;
  llvm::BumpPtrAllocator DebugInfoNames;
  llvm::DICompileUnit TheCU;
  const Options &Opts;
public:
  IRGenDebugInfo(const Options &Opts,
                 llvm::SourceMgr &SM, llvm::Module &M);

  /// Finalize the DIBuilder.
  void finalize();

  /// Update the IRBuilder's current debug location to the location
  /// Loc and the lexical scope DS.
  void setCurrentLoc(IRBuilder &Builder, SILDebugScope *DS,
                     SILLocation Loc = SILLocation());

  /// Create debug info for the given funtion.
  void createFunction(SILDebugScope *DS, llvm::Function *Fn);

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
  /// @param Tag - The DWARF tag that should be used.
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
                                    const llvm::Twine &Name);

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
  llvm::DIFile getOrCreateFile(const char *filename);
};

} // irgen
} // swift

#endif
