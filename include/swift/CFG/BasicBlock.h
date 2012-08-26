//===--- BasicBlock.h - Basic blocks for high-level CFGs ---------*- C++ -*-==//
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
// This file defines the high-level BasicBlocks used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_BASICBLOCK_H
#define SWIFT_CFG_BASICBLOCK_H

#include "swift/CFG/Instruction.h"

namespace swift {
class CFG;

class BasicBlock :
public llvm::ilist_node<BasicBlock>, public CFGAllocated<BasicBlock> {
public:
  typedef llvm::iplist<Instruction> InstListType;
private:
  /// A backreference to the containing CFG.
  CFG * const ParentCFG;
  /// The ordered set of instructions in the BasicBlock.
  InstListType InstList;

  std::vector<BasicBlock *> PredList;

  friend struct llvm::ilist_sentinel_traits<BasicBlock>;
  BasicBlock() : ParentCFG(0) {}
  void operator=(const BasicBlock &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

public:
  BasicBlock(CFG *C);
  ~BasicBlock();

  CFG *getParent() { return ParentCFG; }
  const CFG *getParent() const { return ParentCFG; }

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

  //===--------------------------------------------------------------------===//
  // Predecessors and Successors
  //===--------------------------------------------------------------------===//

  void addPred(BasicBlock *B) { PredList.push_back(B); }

  typedef llvm::ArrayRef<BasicBlock *> Predecessors;
  typedef llvm::ArrayRef<BasicBlock *> Successors;

  /// The predecessors of a BasicBlock are currently represented internally
  /// using an std::vector.  This will be optimized later, as most BasicBlocks
  /// will have only a single predecessor.
  Predecessors getPreds() const { return PredList; }

  TermInst *getTerminator() {
    assert(!InstList.empty() && "Can't get successors for malformed block");
    return cast<TermInst>(&InstList.back());
  }

  const TermInst *getTerminator() const {
    return const_cast<BasicBlock*>(this)->getTerminator();
  }

  /// The successors of a BasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  Successors getSuccs() const {
    return getTerminator()->successors();
  }

  /// Pretty-print the BasicBlock.
  void dump() const;

  /// Pretty-print the BasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;


  /// getSublistAccess() - returns pointer to member of instruction list
  static InstListType BasicBlock::*getSublistAccess() {
    return &BasicBlock::InstList;
  }
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
