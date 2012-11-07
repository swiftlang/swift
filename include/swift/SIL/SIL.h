//===--- CFG.h - Defines the CFG and CFG library umbrella header -*- C++ -*-==//
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
// This file defines the CFG and includes the CFG components headers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CFG_H
#define SWIFT_SIL_CFG_H

#include "swift/SIL/BasicBlock.h"

namespace swift {

class ASTContext;
class FuncExpr;
class Instruction;
class TranslationUnit;

class CFG : public SILBase {
public:
  typedef llvm::iplist<BasicBlock> BlockListType;

private:
  friend class BasicBlock;

  /// Context - This is the context that uniques the types used by this CFG.
  ASTContext &Context;

  /// The collection of all BasicBlocks in the CFG.
  BlockListType BlockList;

  // Intentionally marked private so that we need to use 'constructSIL()'
  // to construct a CFG.
  CFG(ASTContext &Context) : Context(Context) {}
public:
  ~CFG();

  /// Construct a CFG from a given statement.  It is the caller's responsibility
  /// to 'delete' this object.
  static CFG *constructSIL(FuncExpr *FE);

  ASTContext &getContext() const { return Context; }

  //===--------------------------------------------------------------------===//
  // Block List Access
  //===--------------------------------------------------------------------===//

  BlockListType &getBlocks() { return BlockList; }
  const BlockListType &getBlocks() const { return BlockList; }

  typedef BlockListType::iterator iterator;
  typedef BlockListType::const_iterator const_iterator;

  bool empty() { return BlockList.empty(); }
  iterator begin() { return BlockList.begin(); }
  iterator end() { return BlockList.end(); }
  const_iterator begin() const { return BlockList.begin(); }
  const_iterator end() const { return BlockList.end(); }


  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the CFG follows invariants.
  void verify() const;
  
  /// Pretty-print the CFG.
  void dump() const;

  /// Pretty-print the CFG with the designated stream.
  void print(raw_ostream &OS) const;
};

} // end swift namespace

#endif
