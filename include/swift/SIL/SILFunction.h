//===--- SILFunction.h - Defines the SILFunction class ----------*- C++ -*-===//
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
// This file defines the SILFunction class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILFUNCTION_H
#define SWIFT_SIL_SILFUNCTION_H

#include "swift/SIL/SILBasicBlock.h"

namespace swift {

class ASTContext;
class FuncExpr;
class SILInstruction;
class SILModule;
class TranslationUnit;
  
namespace Lowering {
  class SILGenModule;
}

/// Linkage attribute for a SIL function.
enum class SILLinkage : unsigned char {
  External,
  ClangThunk,
  Internal,
};
  
/// SILFunction - A function body that has been lowered to SIL. This consists of
/// zero or more SIL SILBasicBlock objects that contain the SILInstruction
/// objects making up the function.
class SILFunction
  : public llvm::ilist_node<SILFunction>, public SILAllocated<SILFunction> {
public:
  typedef llvm::iplist<SILBasicBlock> BlockListType;

private:
  friend class SILBasicBlock;
  friend class SILModule;

  /// ModuleAndLinkage - The SIL module that the function belongs to, and
  /// the function's linkage.
  llvm::PointerIntPair<SILModule*, 2, SILLinkage> ModuleAndLinkage;
  
  /// MangledName - This is the mangled name of the SIL function, which will
  /// be propagated into LLVM IR.
  std::string MangledName;

  /// The lowered type of the function.
  SILType LoweredType;
  
  /// The collection of all BasicBlocks in the SILFunction. Empty for external
  /// function references.
  BlockListType BlockList;
public:

  SILFunction(SILModule &Module, SILLinkage Linkage,
              StringRef MangledName, SILType LoweredType);
  
  ~SILFunction();

  SILModule &getModule() const { return *ModuleAndLinkage.getPointer(); }
  SILType getLoweredType() const { return LoweredType; }
  SILFunctionTypeInfo *getFunctionTypeInfo() const {
    return LoweredType.getFunctionTypeInfo();
  }
  
  /// Returns the uncurry level of this entry point.
  unsigned getUncurryLevel() const {
    return getFunctionTypeInfo()->getUncurryLevel();
  }
  
  /// Returns the calling convention used by this entry point.
  AbstractCC getAbstractCC() const {
    return getFunctionTypeInfo()->getAbstractCC();
  }

  StringRef getMangledName() const { return MangledName; }
  void setMangledName(StringRef N) {
    MangledName = N;
  }
  
  std::string &getMutableMangledName() { return MangledName; }

  /// True if this is a declaration of a function defined in another module.
  bool isExternalDeclaration() const { return BlockList.empty(); }
  
  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return ModuleAndLinkage.getInt(); }
  
  //===--------------------------------------------------------------------===//
  // Block List Access
  //===--------------------------------------------------------------------===//

  BlockListType &getBlocks() { return BlockList; }
  const BlockListType &getBlocks() const { return BlockList; }

  typedef BlockListType::iterator iterator;
  typedef BlockListType::const_iterator const_iterator;

  bool empty() const { return BlockList.empty(); }
  iterator begin() { return BlockList.begin(); }
  iterator end() { return BlockList.end(); }
  const_iterator begin() const { return BlockList.begin(); }
  const_iterator end() const { return BlockList.end(); }
  unsigned size() const { return BlockList.size(); }

  SILBasicBlock &front() { return *begin(); }
  const SILBasicBlock &front() const { return *begin(); }

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the SILFunction follows
  /// invariants.
  void verify() const;
  
  /// Pretty-print the SILFunction.
  void dump() const;

  /// Pretty-print the SILFunction with the designated stream as a 'sil'
  /// definition.
  void print(raw_ostream &OS) const;
  
  /// Pretty-print the SILFunction's name using SIL syntax,
  /// '@function_mangled_name'.
  void printName(raw_ostream &OS) const;
  
  ASTContext &getASTContext() const;
};

/// Observe that we are processing a specific SIL function.
class PrettyStackTraceSILFunction : public llvm::PrettyStackTraceEntry {
  SILFunction *F;
  const char *Action;
public:
  PrettyStackTraceSILFunction(const char *Action, SILFunction *F)
  : F(F), Action(Action) {}
  virtual void print(raw_ostream &OS) const;
};

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILFunction
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILFunction> :
public ilist_default_traits<::swift::SILFunction> {
  typedef ::swift::SILFunction SILFunction;

private:
  mutable ilist_half_node<SILFunction> Sentinel;

public:
  SILFunction *createSentinel() const {
    return static_cast<SILFunction*>(&Sentinel);
  }
  void destroySentinel(SILFunction *) const {}

  SILFunction *provideInitialHead() const { return createSentinel(); }
  SILFunction *ensureHead(SILFunction*) const { return createSentinel(); }
  static void noteHead(SILFunction*, SILFunction*) {}
  static void deleteNode(SILFunction *V) {}
  
private:
  void createNode(const SILFunction &);
};

} // end llvm namespace

#endif
