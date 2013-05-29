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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/ilist.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
  class TranslationUnit;
  class ASTContext;
  class FuncDecl;
  class SILFunction;
  class SILTypeList;
  class AnyFunctionType;
  
  namespace Lowering {
    class SILGenModule;
  }
  
/// SILModule - A SIL translation unit. The module object owns all of the SIL
/// SILFunction and other top-level objects generated when a translation unit is
/// lowered to SIL.
class SILModule {
public:
  typedef llvm::ilist<SILFunction> FunctionListType;
  
private:
  friend class SILBasicBlock;
  friend class SILFunction;
  friend class Lowering::SILGenModule;
  friend class Lowering::TypeConverter;
  friend class SILType;

  /// Allocator that manages the memory of all the pieces of the SILModule.
  mutable llvm::BumpPtrAllocator BPA;
  void *TypeListUniquing;

  /// Context - This is the context that uniques the types used by this
  /// SILFunction.
  ASTContext &TheASTContext;
  
  /// The list of Functions in the module.
  FunctionListType functions;

  /// The collection of global variables used in the module.
  llvm::SetVector<VarDecl*> globals;

  /// This is a cache that memoizes the result of SILType::isAddressOnly.
  llvm::DenseMap<TypeBase*, bool> AddressOnlyTypeCache;
  
  /// This is a cache that memoizes the result of SILType::getFunctionTypeInfo.
  llvm::DenseMap<AnyFunctionType*, SILFunctionTypeInfo*> FunctionTypeInfoCache;
  
  /// Derive the SILFunctionTypeInfo for a type.
  SILFunctionTypeInfo *makeFunctionTypeInfo(AnyFunctionType *ft);
  
  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a SILModule.
  SILModule(ASTContext &TheASTContext);
  
  SILModule(const SILModule&) = delete;
  void operator=(const SILModule&) = delete;

public:
  ~SILModule();

  /// getSILTypeList - Get a uniqued pointer to a SIL type list.
  SILTypeList *getSILTypeList(llvm::ArrayRef<SILType> Types) const;

  /// Types - This converts Swift types to SILTypes.
  Lowering::TypeConverter Types;
  
  /// Construct a SIL module from a translation unit.  It is the caller's
  /// responsibility to 'delete' this object.
  static SILModule *constructSIL(TranslationUnit *tu,
                                 unsigned startElem);

  /// createEmptyModule - Create and return an empty SIL module that we can
  /// later parse SIL bodies directly into, without converting from an AST.
  static SILModule *createEmptyModule(ASTContext &Context) {
    return new SILModule(Context);
  }
  
  ASTContext &getASTContext() const { return TheASTContext; }
  
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
  
  typedef FunctionListType::iterator iterator;
  typedef FunctionListType::const_iterator const_iterator;
  
  iterator begin() { return functions.begin(); }
  iterator end() { return functions.end(); }
  const_iterator begin() const { return functions.begin(); }
  const_iterator end() const { return functions.end(); }

  /// verify - Run the SIL verifier to make sure that all Functions follow
  /// invariants.
  void verify() const;
  
  /// Pretty-print the module.
  void dump() const;

  /// Pretty-print the module to the designated stream.
  void print(raw_ostream &OS) const;
  
  /// Allocate memory using Function's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const {
    return BPA.Allocate(Size, Align);
  }
};

} // end swift namespace

#endif
