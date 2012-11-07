//===--- BasicBlock.h - Basic blocks for SIL --------------------*- C++ -*-===//
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
// This file defines the high-level BasicBlocks used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_BASICBLOCK_H
#define SWIFT_SIL_BASICBLOCK_H

#include "swift/SIL/Instruction.h"

namespace swift {
class CFG;
class BBArgument;

class BasicBlock :
public llvm::ilist_node<BasicBlock>, public SILAllocated<BasicBlock> {
  friend class SILSuccessor;
public:
  typedef llvm::iplist<Instruction> InstListType;
private:
  /// A backreference to the containing CFG.
  CFG * const Parent;
  
  /// PrevList - This is a list of all of the terminator operands that are
  /// branching to this block, forming the predecessor list.  This is
  /// automatically managed by the SILSuccessor class.
  SILSuccessor *PredList;

  /// BBArgList - This is the list of basic block arguments for this block.
  std::vector<BBArgument*> BBArgList;

  /// The ordered set of instructions in the BasicBlock.
  InstListType InstList;
  
  friend struct llvm::ilist_sentinel_traits<BasicBlock>;
  BasicBlock() : Parent(0) {}
  void operator=(const BasicBlock &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

public:
  BasicBlock(CFG *F, const char *Name = "");
  ~BasicBlock();

  CFG *getParent() { return Parent; }
  const CFG *getParent() const { return Parent; }

  /// eraseFromParent - This method unlinks 'this' from the containing Function
  /// and deletes it.
  ///
  void eraseFromParent();

  //===--------------------------------------------------------------------===//
  // Instruction List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  InstListType &getInsts() { return InstList; }
  const InstListType &getInsts() const { return InstList; }

  typedef InstListType::iterator iterator;
  typedef InstListType::const_iterator const_iterator;

  bool empty() const { return InstList.empty(); }
  iterator begin() { return InstList.begin(); }
  iterator end() { return InstList.end(); }
  const_iterator begin() const { return InstList.begin(); }
  const_iterator end() const { return InstList.end(); }

  TermInst *getTerminator() {
    assert(!InstList.empty() && "Can't get successors for malformed block");
    return cast<TermInst>(&InstList.back());
  }
  
  const TermInst *getTerminator() const {
    return const_cast<BasicBlock*>(this)->getTerminator();
  }

  //===--------------------------------------------------------------------===//
  // BasicBlock Argument List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  typedef std::vector<BBArgument*>::iterator bbarg_iterator;
  typedef std::vector<BBArgument*>::const_iterator const_bbarg_iterator;

  bool bbarg_empty() const { return BBArgList.empty(); }
  bbarg_iterator bbarg_begin() { return BBArgList.begin(); }
  bbarg_iterator bbarg_end() { return BBArgList.end(); }
  const_bbarg_iterator bbarg_begin() const { return BBArgList.begin(); }
  const_bbarg_iterator bbarg_end() const { return BBArgList.end(); }

  //===--------------------------------------------------------------------===//
  // Predecessors and Successors
  //===--------------------------------------------------------------------===//

  typedef SILSuccessorIterator pred_iterator;
  
  bool pred_empty() const { return PredList == nullptr; }
  pred_iterator pred_begin() const { return pred_iterator(PredList); }
  pred_iterator pred_end() const { return pred_iterator(); }
     
  typedef ArrayRef<SILSuccessor> Successors;

  /// The successors of a BasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  Successors getSuccs() const {
    return getTerminator()->getSuccessors();
  }

  /// Pretty-print the BasicBlock.
  void dump() const;

  /// Pretty-print the BasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;


  /// getSublistAccess() - returns pointer to member of instruction list
  static InstListType BasicBlock::*getSublistAccess() {
    return &BasicBlock::InstList;
  }

private:
  friend class BBArgument;
  /// BBArgument's ctor adds it to the argument list of this block.
  void addArgument(BBArgument *Arg) { BBArgList.push_back(Arg); }
};

} // end swift namespace

namespace llvm {

  raw_ostream &operator<<(raw_ostream &, const swift::BasicBlock &B);

//===----------------------------------------------------------------------===//
// ilist_traits for BasicBlock
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::BasicBlock> :
public ilist_default_traits<::swift::BasicBlock> {
  typedef ::swift::BasicBlock BasicBlock;

private:
  mutable ilist_half_node<BasicBlock> Sentinel;

public:
  BasicBlock *createSentinel() const {
    return static_cast<BasicBlock*>(&Sentinel);
  }
  void destroySentinel(BasicBlock *) const {}

  BasicBlock *provideInitialHead() const { return createSentinel(); }
  BasicBlock *ensureHead(BasicBlock*) const { return createSentinel(); }
  static void noteHead(BasicBlock*, BasicBlock*) {}
  static void deleteNode(BasicBlock *V) {}
  
private:
  void createNode(const BasicBlock &);
};

} // end llvm namespace

#endif
