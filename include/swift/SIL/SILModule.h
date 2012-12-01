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

#include "swift/SIL/SILBase.h"
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/DenseMap.h>

namespace swift {
  class TranslationUnit;
  class ASTContext;
  class FuncDecl;
  
  namespace Lowering {
    class SILGenModule;
  }

class SILModule : public SILBase {
public:
  typedef llvm::iplist<BasicBlock> BlockListType;

private:
  friend class BasicBlock;
  friend class Function;
  friend class Lowering::SILGenModule;

  /// Context - This is the context that uniques the types used by this
  /// Function.
  ASTContext &Context;

  /// The collection of all Functions in the module.
  llvm::DenseMap<ValueDecl*, Function*> functions;
  /// FIXME a hack so that SILModule::print dumps functions in source order
  std::vector<ValueDecl*> functionDecls;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(ASTContext &Context) : Context(Context) {}
public:
  ~SILModule();

  /// Construct a SIL module from a translation unit.  It is the caller's
  /// responsibility to 'delete' this object.
  static SILModule *constructSIL(TranslationUnit *tu);

  ASTContext &getContext() const { return Context; }
  
  llvm::ArrayRef<ValueDecl*> getFunctionDecls() const {
    return functionDecls;
  }
  
  llvm::DenseMap<ValueDecl*, Function*> const &getFunctions() const {
    return functions;
  }

  /// verify - Run the SIL verifier to make sure that all Functions follow
  /// invariants.
  void verify() const;
  
  /// Pretty-print the module.
  void dump() const;

  /// Pretty-print the module to the designated stream.
  void print(raw_ostream &OS) const;
};

} // end swift namespace

#endif
