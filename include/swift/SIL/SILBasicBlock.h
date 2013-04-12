//===--- SILBasicBlock.h - Basic blocks for SIL -----------------*- C++ -*-===//
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

#include "swift/SIL/SILInstruction.h"

namespace llvm {
  template <class T> struct GraphTraits;
};

namespace swift {
class SILFunction;
class SILArgument;

class SILBasicBlock :
public llvm::ilist_node<SILBasicBlock>, public SILAllocated<SILBasicBlock> {
  friend class SILSuccessor;
public:
  typedef llvm::iplist<SILInstruction> InstListType;
private:
  /// A backreference to the containing SILFunction.
  SILFunction * const Parent;
  
  /// PrevList - This is a list of all of the terminator operands that are
  /// branching to this block, forming the predecessor list.  This is
  /// automatically managed by the SILSuccessor class.
  SILSuccessor *PredList;

  /// BBArgList - This is the list of basic block arguments for this block.
  std::vector<SILArgument*> BBArgList;

  /// The ordered set of instructions in the SILBasicBlock.
  InstListType InstList;
  
  friend struct llvm::ilist_sentinel_traits<SILBasicBlock>;
  SILBasicBlock() : Parent(0) {}
  void operator=(const SILBasicBlock &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

public:
  SILBasicBlock(SILFunction *F, const char *Name = "");
  ~SILBasicBlock();

  SILFunction *getParent() { return Parent; }
  const SILFunction *getParent() const { return Parent; }

  /// eraseFromParent - This method unlinks 'this' from the containing
  /// SILFunction and deletes it.
  ///
  void eraseFromParent();

  //===--------------------------------------------------------------------===//
  // SILInstruction List Inspection and Manipulation
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
    return const_cast<SILBasicBlock*>(this)->getTerminator();
  }

  //===--------------------------------------------------------------------===//
  // SILBasicBlock Argument List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  typedef std::vector<SILArgument*>::iterator bbarg_iterator;
  typedef std::vector<SILArgument*>::const_iterator const_bbarg_iterator;

  bool bbarg_empty() const { return BBArgList.empty(); }
  size_t bbarg_size() const { return BBArgList.size(); }
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

  /// The successors of a SILBasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  Successors getSuccs() const {
    return getTerminator()->getSuccessors();
  }

  /// Pretty-print the SILBasicBlock.
  void dump() const;

  /// Pretty-print the SILBasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;


  /// getSublistAccess() - returns pointer to member of instruction list
  static InstListType SILBasicBlock::*getSublistAccess() {
    return &SILBasicBlock::InstList;
  }

private:
  friend class SILArgument;
  /// BBArgument's ctor adds it to the argument list of this block.
  void addArgument(SILArgument *Arg) { BBArgList.push_back(Arg); }
};

void WriteAsOperand(raw_ostream &out, swift::SILBasicBlock *BB,
                    bool printType = true);

} // end swift namespace

namespace llvm {

//===----------------------------------------------------------------------===//
// ilist_traits for SILBasicBlock
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::SILBasicBlock> :
public ilist_default_traits<::swift::SILBasicBlock> {
  typedef ::swift::SILBasicBlock SILBasicBlock;

private:
  mutable ilist_half_node<SILBasicBlock> Sentinel;

public:
  SILBasicBlock *createSentinel() const {
    return static_cast<SILBasicBlock*>(&Sentinel);
  }
  void destroySentinel(SILBasicBlock *) const {}

  SILBasicBlock *provideInitialHead() const { return createSentinel(); }
  SILBasicBlock *ensureHead(SILBasicBlock*) const { return createSentinel(); }
  static void noteHead(SILBasicBlock*, SILBasicBlock*) {}
  static void deleteNode(SILBasicBlock *V) {}
  
private:
  void createNode(const SILBasicBlock &);
};

} // end llvm namespace

#endif
