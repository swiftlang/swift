//===--- SILBasicBlock.h - Basic blocks for SIL -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

#include "swift/Basic/Range.h"
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
  friend class SILFunction;
public:
  using InstListType = llvm::iplist<SILInstruction>;
private:
  /// A backreference to the containing SILFunction.
  SILFunction *Parent;

  /// PrevList - This is a list of all of the terminator operands that are
  /// branching to this block, forming the predecessor list.  This is
  /// automatically managed by the SILSuccessor class.
  SILSuccessor *PredList;

  /// BBArgList - This is the list of basic block arguments for this block.
  std::vector<SILArgument*> BBArgList;

  /// The ordered set of instructions in the SILBasicBlock.
  InstListType InstList;

  friend struct llvm::ilist_sentinel_traits<SILBasicBlock>;
  friend struct llvm::ilist_traits<SILBasicBlock>;
  SILBasicBlock() : Parent(0) {}
  void operator=(const SILBasicBlock &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

public:
  SILBasicBlock(SILFunction *F, SILBasicBlock *afterBB = nullptr);
  ~SILBasicBlock();

  /// Gets the ID (= index in the function's block list) of the block.
  ///
  /// Returns -1 if the block is not contained in a function.
  /// Warning: This function is slow. Therefore it should only be used for
  ///          debug output.
  int getDebugID();

  SILFunction *getParent() { return Parent; }
  const SILFunction *getParent() const { return Parent; }

  SILModule &getModule() const;

  /// This method unlinks 'self' from the containing SILFunction and deletes it.
  void eraseFromParent();

  /// Returns true if this BB is the entry BB of its parent.
  bool isEntry() const;

  //===--------------------------------------------------------------------===//
  // SILInstruction List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  using iterator = InstListType::iterator;
  using const_iterator = InstListType::const_iterator;
  using reverse_iterator = InstListType::reverse_iterator;
  using const_reverse_iterator = InstListType::const_reverse_iterator;

  void insert(iterator InsertPt, SILInstruction *I);
  void insert(SILInstruction *InsertPt, SILInstruction *I) {
    insert(InsertPt->getIterator(), I);
  }

  void push_back(SILInstruction *I);
  void push_front(SILInstruction *I);
  void remove(SILInstruction *I);
  void erase(SILInstruction *I);

  SILInstruction &back() { return InstList.back(); }
  const SILInstruction &back() const {
    return const_cast<SILBasicBlock *>(this)->back();
  }

  SILInstruction &front() { return InstList.front(); }
  const SILInstruction &front() const {
    return const_cast<SILBasicBlock *>(this)->front();
  }

  /// Transfer the instructions from Other to the end of this block.
  void spliceAtEnd(SILBasicBlock *Other) {
    InstList.splice(end(), Other->InstList);
  }

  bool empty() const { return InstList.empty(); }
  iterator begin() { return InstList.begin(); }
  iterator end() { return InstList.end(); }
  const_iterator begin() const { return InstList.begin(); }
  const_iterator end() const { return InstList.end(); }
  reverse_iterator rbegin() { return InstList.rbegin(); }
  reverse_iterator rend() { return InstList.rend(); }
  const_reverse_iterator rbegin() const { return InstList.rbegin(); }
  const_reverse_iterator rend() const { return InstList.rend(); }

  TermInst *getTerminator() {
    assert(!InstList.empty() && "Can't get successors for malformed block");
    return cast<TermInst>(&InstList.back());
  }

  const TermInst *getTerminator() const {
    return const_cast<SILBasicBlock*>(this)->getTerminator();
  }

  /// \brief Splits a basic block into two at the specified instruction.
  ///
  /// Note that all the instructions BEFORE the specified iterator
  /// stay as part of the original basic block. The old basic block is left
  /// without a terminator.
  SILBasicBlock *splitBasicBlock(iterator I);

  /// \brief Move the basic block to after the specified basic block in the IR.
  /// The basic blocks must reside in the same function.
  void moveAfter(SILBasicBlock *After);

  //===--------------------------------------------------------------------===//
  // SILBasicBlock Argument List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  using bbarg_iterator = std::vector<SILArgument *>::iterator;
  using const_bbarg_iterator = std::vector<SILArgument *>::const_iterator;

  bool bbarg_empty() const { return BBArgList.empty(); }
  size_t bbarg_size() const { return BBArgList.size(); }
  bbarg_iterator bbarg_begin() { return BBArgList.begin(); }
  bbarg_iterator bbarg_end() { return BBArgList.end(); }
  const_bbarg_iterator bbarg_begin() const { return BBArgList.begin(); }
  const_bbarg_iterator bbarg_end() const { return BBArgList.end(); }

  ArrayRef<SILArgument*> getBBArgs() const { return BBArgList; }

  unsigned getNumBBArg() const { return BBArgList.size(); }
  const SILArgument *getBBArg(unsigned i) const { return BBArgList[i]; }
  SILArgument *getBBArg(unsigned i) { return BBArgList[i]; }

  /// Replace the \p{i}th BB arg with a new BBArg with SILType \p Ty and ValueDecl
  /// \p D.
  SILArgument *replaceBBArg(unsigned i, SILType Ty, const ValueDecl *D=nullptr);

  /// Erase a specific argument from the arg list.
  void eraseBBArg(int Index);

  /// Allocate a new argument of type \p Ty and append it to the argument
  /// list. Optionally you can pass in a value decl parameter.
  SILArgument *createBBArg(SILType Ty, const ValueDecl *D=nullptr);

  /// Insert a new SILArgument with type \p Ty and \p Decl at position \p Pos.
  SILArgument *insertBBArg(bbarg_iterator Pos, SILType Ty,
                           const ValueDecl *D=nullptr);

  SILArgument *insertBBArg(unsigned Index, SILType Ty,
                           const ValueDecl *D=nullptr) {
    bbarg_iterator Pos = BBArgList.begin();
    std::advance(Pos, Index);
    return insertBBArg(Pos, Ty, D);
  }

  /// \brief Remove all block arguments.
  void dropAllBBArgs() { BBArgList.clear(); }

  /// \brief Drops all uses that belong to this basic block.
  void dropAllReferences() {
    dropAllBBArgs();
    for (SILInstruction &I : *this)
      I.dropAllReferences();
  }

  //===--------------------------------------------------------------------===//
  // Predecessors and Successors
  //===--------------------------------------------------------------------===//

  using SuccessorListTy = TermInst::SuccessorListTy;
  using ConstSuccessorListTy = TermInst::ConstSuccessorListTy;
  
  /// The successors of a SILBasicBlock are defined either explicitly as
  /// a single successor as the branch targets of the terminator instruction.
  ConstSuccessorListTy getSuccessors() const {
    return getTerminator()->getSuccessors();
  }
  SuccessorListTy getSuccessors() {
    return getTerminator()->getSuccessors();
  }

  using const_succ_iterator = ConstSuccessorListTy::const_iterator;
  using succ_iterator = SuccessorListTy::iterator;

  bool succ_empty() const { return getSuccessors().empty(); }
  succ_iterator succ_begin() { return getSuccessors().begin(); }
  succ_iterator succ_end() { return getSuccessors().end(); }
  const_succ_iterator succ_begin() const { return getSuccessors().begin(); }
  const_succ_iterator succ_end() const { return getSuccessors().end(); }

  SILBasicBlock *getSingleSuccessor() {
    if (succ_empty() || std::next(succ_begin()) != succ_end())
      return nullptr;
    return *succ_begin();
  }

  const SILBasicBlock *getSingleSuccessor() const {
    if (succ_empty() || std::next(succ_begin()) != succ_end())
      return nullptr;
    return *succ_begin();
  }

  /// \brief Returns true if \p BB is a successor of this block.
  bool isSuccessor(SILBasicBlock *BB) const {
    auto Range = getSuccessorBlocks();
    return any_of(Range, [&BB](const SILBasicBlock *SuccBB) -> bool {
      return BB == SuccBB;
    });
  }

  using SuccessorBlockListTy =
    TransformRange<SuccessorListTy,
                   std::function<SILBasicBlock *(const SILSuccessor &)>>;
  using ConstSuccessorBlockListTy =
    TransformRange<ConstSuccessorListTy,
                   std::function<const SILBasicBlock *(const SILSuccessor &)>>;

  /// Return the range of SILBasicBlocks that are successors of this block.
  SuccessorBlockListTy getSuccessorBlocks() {
    using FuncTy = std::function<SILBasicBlock *(const SILSuccessor &)>;
    FuncTy F(&SILSuccessor::getBB);
    return makeTransformRange(getSuccessors(), F);
  }

  /// Return the range of SILBasicBlocks that are successors of this block.
  ConstSuccessorBlockListTy getSuccessorBlocks() const {
    using FuncTy = std::function<const SILBasicBlock *(const SILSuccessor &)>;
    FuncTy F(&SILSuccessor::getBB);
    return makeTransformRange(getSuccessors(), F);
  }

  using pred_iterator = SILSuccessorIterator;

  bool pred_empty() const { return PredList == nullptr; }
  pred_iterator pred_begin() const { return pred_iterator(PredList); }
  pred_iterator pred_end() const { return pred_iterator(); }

  iterator_range<pred_iterator> getPreds() const {
    return {pred_begin(), pred_end() };
  }

  bool isPredecessor(SILBasicBlock *BB) const {
    return any_of(getPreds(), [&BB](const SILBasicBlock *PredBB) -> bool {
      return BB == PredBB;
    });
  }

  SILBasicBlock *getSinglePredecessor() {
    if (pred_empty() || std::next(pred_begin()) != pred_end())
      return nullptr;
    return *pred_begin();
  }

  const SILBasicBlock *getSinglePredecessor() const {
    return const_cast<SILBasicBlock*>(this)->getSinglePredecessor();
  }

  /// Pretty-print the SILBasicBlock.
  void dump() const;

  /// Pretty-print the SILBasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;

  void printAsOperand(raw_ostream &OS, bool PrintType = true);

  /// getSublistAccess() - returns pointer to member of instruction list
  static InstListType SILBasicBlock::*getSublistAccess() {
    return &SILBasicBlock::InstList;
  }

private:
  friend class SILArgument;

  /// BBArgument's ctor adds it to the argument list of this block.
  void insertArgument(bbarg_iterator Iter, SILArgument *Arg) {
    BBArgList.insert(Iter, Arg);
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const SILBasicBlock &BB) {
  BB.print(OS);
  return OS;
}
} // end swift namespace

namespace llvm {

//===----------------------------------------------------------------------===//
// ilist_traits for SILBasicBlock
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::SILBasicBlock> :
public ilist_default_traits<::swift::SILBasicBlock> {
  using SelfTy = ilist_traits<::swift::SILBasicBlock>;
  using SILBasicBlock = ::swift::SILBasicBlock;
  using SILFunction = ::swift::SILFunction;
  using FunctionPtrTy = ::swift::NullablePtr<SILFunction>;

private:
  friend class ::swift::SILFunction;
  mutable ilist_half_node<SILBasicBlock> Sentinel;

  SILFunction *Parent;

public:

  SILBasicBlock *createSentinel() const {
    return static_cast<SILBasicBlock*>(&Sentinel);
  }
  void destroySentinel(SILBasicBlock *) const {}

  SILBasicBlock *provideInitialHead() const { return createSentinel(); }
  SILBasicBlock *ensureHead(SILBasicBlock*) const { return createSentinel(); }
  static void noteHead(SILBasicBlock*, SILBasicBlock*) {}
  static void deleteNode(SILBasicBlock *BB) { BB->~SILBasicBlock(); }

  void addNodeToList(SILBasicBlock *BB) {
  }

  void transferNodesFromList(ilist_traits<SILBasicBlock> &SrcTraits,
                             ilist_iterator<SILBasicBlock> First,
                             ilist_iterator<SILBasicBlock> Last);

private:
  static void createNode(const SILBasicBlock &);
};

} // end llvm namespace

#endif
