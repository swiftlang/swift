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

#ifndef SWIFT_BASICBLOCK_H
#define SWIFT_BASICBLOCK_H

#include "Instruction.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include <vector>

namespace swift {

class CFG;

#define DEF_ITERATOR(ITER, ADVANCE, START, BEGIN, END, ...)\
class ITER {\
  __VA_ARGS__ Instruction *I;\
public:\
  ITER( __VA_ARGS__ Instruction *I = 0) : I(I) {}\
  bool operator==(const ITER &X) const { return I == X.I; };\
  bool operator!=(const ITER &X) const { return I != X.I; };\
  ITER &operator++() { I = I->ADVANCE##Inst; return *this; }\
  __VA_ARGS__ Instruction *operator*() const { return I; }\
  __VA_ARGS__ Instruction &operator->() const { return *I; }\
};\
ITER BEGIN () __VA_ARGS__ { return START; }\
ITER END () __VA_ARGS__ { return nullptr; }

class BasicBlock {
  // FIXME: Optimize Preds representation later.
  std::vector<BasicBlock *> Preds;
  Instruction *FrontI, *BackI;
  llvm::PointerUnion<BasicBlock*, TermInst *> SuccOrTerm;

private:
  BasicBlock(); // Do not implement.
  BasicBlock(const BasicBlock &B); // Do not implement.

public:
  /// A backreference to the containing CFG.
  CFG * const cfg;

  /// The numeric identifier for the block.
  const unsigned blockID;

  BasicBlock(CFG *C, unsigned &BlockID);

  ~BasicBlock();

  DEF_ITERATOR(iterator, next, FrontI, begin, end, )
  DEF_ITERATOR(const_iterator, next, FrontI, begin, end, const)
  DEF_ITERATOR(reverse_iterator, prev, BackI, rbegin, rend, )
  DEF_ITERATOR(const_reverse_iterator, prev, BackI, rbegin, rend, const)

  /// Return the instruction at the entry of the basic block.
  Instruction *front() const { return FrontI; }

  /// Return the instruction at the end of the basic block, but before the
  /// terminator.
  Instruction *back() const { return BackI; }

  /// Return true if the block is empty (excluding terminator).
  bool empty() { return front() == nullptr; }

  /// Return the terminator instruction (if any).
  TermInst *terminator() const { return SuccOrTerm.dyn_cast<TermInst*>(); }

  /// Pretty-print the BasicBlock.
  void dump() const;

  /// Pretty-print the BasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;

  void addPred(BasicBlock *B) { Preds.push_back(B); }
  void setFront(Instruction *I) { FrontI = I; }
  void setBack(Instruction *I) { BackI = I; }
  void setTerm(TermInst *I) {
    assert(SuccOrTerm.isNull());
    SuccOrTerm = I;
  }
  void setSucc(BasicBlock *Succ) {
    assert(SuccOrTerm.isNull());
    SuccOrTerm = Succ;
  }

  typedef llvm::ArrayRef<BasicBlock *> Predecessors;
  typedef llvm::ArrayRef<BasicBlock *> Successors;

  /// The predecessors of a BasicBlock are currently represented internally
  /// using an std::vector.  This will be optimized later, as most BasicBlocks
  /// will have only a single predecessor.
  Predecessors preds() { return Preds; }
  const Predecessors preds() const { return Preds; }

  /// The successors of a BasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  Successors succs() {
    if (SuccOrTerm.isNull())
      return nullptr;
    if (TermInst *T = terminator())
      return T->branchTargets();
    BasicBlock **Succ = SuccOrTerm.getAddrOfPtr1();
    return llvm::ArrayRef<BasicBlock*>(Succ, Succ);
  }
  const Successors succs() const {
    return const_cast<BasicBlock*>(this)->succs();
  }
};

} // end swift namespace

namespace llvm {
#define DEF_GRAPHTRAIT(BLOCK_TYPE)\
template <> struct GraphTraits< BLOCK_TYPE *> {\
  typedef BLOCK_TYPE NodeType;\
  typedef BLOCK_TYPE::Successors::const_iterator ChildIteratorType;\
  static NodeType *getEntryNode(BLOCK_TYPE *B) { return B; }\
  static inline ChildIteratorType child_begin(NodeType *N) {\
    return N->succs().begin();\
  }\
  static inline ChildIteratorType child_end(NodeType *N) {\
    return N->succs().end();\
  }\
};\
template <> struct GraphTraits<Inverse< BLOCK_TYPE *> > {\
  typedef BLOCK_TYPE NodeType;\
  typedef BLOCK_TYPE::Predecessors::const_iterator ChildIteratorType;\
  static NodeType *getEntryNode(Inverse< BLOCK_TYPE *> G) { return G.Graph; }\
  static inline ChildIteratorType child_begin(NodeType *N) {\
    return N->preds().begin();\
  }\
  static inline ChildIteratorType child_end(NodeType *N) {\
    return N->preds().end();\
  }\
};

DEF_GRAPHTRAIT(::swift::BasicBlock)
DEF_GRAPHTRAIT(const ::swift::BasicBlock)

} // end llvm namespace

#undef DEF_GRAPHTRAIT
#undef DEF_ITERATOR
#endif
