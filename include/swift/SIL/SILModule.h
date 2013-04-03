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

#include "swift/Basic/Range.h"
#include "swift/SIL/SILBase.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
  class TranslationUnit;
  class ASTContext;
  class FuncDecl;
  class Function;
  
  namespace Lowering {
    class SILGenModule;
    class TypeConverter;
  }
  
/// SILModule - A SIL translation unit. The module object owns all of the SIL
/// Function and other top-level objects generated when a translation unit is
/// lowered to SIL.
class SILModule : public SILBase {
private:
  friend class BasicBlock;
  friend class Function;
  friend class Lowering::SILGenModule;
  friend class Lowering::TypeConverter;

  /// Context - This is the context that uniques the types used by this
  /// Function.
  ASTContext &Context;
  
  /// The collection of all codegenned Functions in the module.
  llvm::MapVector<SILConstant, Function*> functions;

  /// The collection of global variables used in the module.
  llvm::DenseSet<VarDecl*> globals;
  
  /// The top-level Function for the module.
  Function *toplevel;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(ASTContext &Context, bool hasTopLevel);
  
public:
  ~SILModule();

  /// Construct a SIL module from a translation unit.  It is the caller's
  /// responsibility to 'delete' this object.
  static SILModule *constructSIL(TranslationUnit *tu, unsigned startElem);

  /// createEmptyModule - Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static SILModule *createEmptyModule(ASTContext &Context) {
    return new SILModule(Context, false);
  }
  
  ASTContext &getContext() const { return Context; }
  
  /// Returns true if this module has top-level code.
  bool hasTopLevelFunction() const {
    return toplevel != nullptr;
  }
  
  /// Returns the Function containing top-level code for the module.
  Function *getTopLevelFunction() const {
    assert(toplevel && "no toplevel");
    return toplevel;
  }
  
  /// Returns true if a Function was generated from the given declaration.
  bool hasFunction(SILConstant decl) const {
    return functions.find(decl) != functions.end();
  }
  
  /// Returns true if a Function was generated from the given declaration.
  bool hasFunction(ValueDecl *decl) const {
    return hasFunction(SILConstant(decl));
  }
  
  /// Returns a pointer to the Function generated from the given declaration.
  Function *getFunction(SILConstant constant) const {
    auto found = functions.find(constant);
    assert(found != functions.end() && "no Function generated for Decl");
    return found->second;
  }

  /// Returns a pointer to the Function generated from the given declaration.
  Function *getFunction(ValueDecl *decl) const {
    return getFunction(SILConstant(decl));
  }
  
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
  
  typedef llvm::MapVector<SILConstant, Function*>::const_iterator iterator;
  typedef iterator const_iterator;
  
  iterator begin() const { return functions.begin(); }
  iterator end() const { return functions.end(); }

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
