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

namespace llvm {
  template<class GraphType> struct GraphTraits;
}

namespace swift {

class CFG;

class BasicBlock : public llvm::ilist_node<BasicBlock>,
                   public CFGAllocated<BasicBlock>  {
public:
  typedef llvm::iplist<Instruction> InstListType;

  /// The ordered set of instructions in the BasicBlock.
  InstListType instructions;

  /// A backreference to the containing CFG.
  CFG * const cfg;

private:
  friend struct llvm::ilist_sentinel_traits<BasicBlock>;
  BasicBlock() : cfg(0) {}
  void operator=(const BasicBlock &) =delete;
  void operator delete(void *Ptr, size_t) = delete;

  std::vector<BasicBlock *> Preds;

public:
  BasicBlock(CFG *C);
  ~BasicBlock();

  /// Pretty-print the BasicBlock.
  void dump() const;

  /// Pretty-print the BasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS, CFGPrintContext &PC,
             unsigned Indent = 0) const;

  void addPred(BasicBlock *B) { Preds.push_back(B); }

  typedef llvm::ArrayRef<BasicBlock *> Predecessors;
  typedef llvm::ArrayRef<BasicBlock *> Successors;

  /// The predecessors of a BasicBlock are currently represented internally
  /// using an std::vector.  This will be optimized later, as most BasicBlocks
  /// will have only a single predecessor.
  Predecessors preds() { return Preds; }
  const Predecessors preds() const { return Preds; }

  TermInst *getTerminator() {
    assert(!instructions.empty() && "Can't get successors for malformed block");
    return cast<TermInst>(&instructions.back());
  }

  /// The successors of a BasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  Successors succs() {
    return getTerminator()->successors();
  }
  const Successors succs() const {
    return const_cast<BasicBlock*>(this)->succs();
  }
};

} // end swift namespace

namespace llvm {

template <> struct GraphTraits<::swift::BasicBlock*> {
  typedef ::swift::BasicBlock NodeType;
  typedef ::swift::BasicBlock::Successors::iterator ChildIteratorType;
  static NodeType *getEntryNode(::swift::BasicBlock *BB) { return BB; }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->succs().begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) {
    return N->succs().end();
  }
};

template <> struct GraphTraits<const ::swift::BasicBlock*> {
  typedef const ::swift::BasicBlock NodeType;
  typedef ::swift::BasicBlock::Successors::const_iterator ChildIteratorType;
  static NodeType *getEntryNode(const ::swift::BasicBlock *BB) { return BB; }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->succs().begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) {
    return N->succs().end();
  }

};

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
