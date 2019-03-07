//===--- SILBasicBlock.h - Basic blocks for SIL -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the high-level BasicBlocks used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_BASICBLOCK_H
#define SWIFT_SIL_BASICBLOCK_H

#include "swift/Basic/Compiler.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/SIL/SILArgumentArrayRef.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

class SILFunction;
class SILArgument;
class SILPhiArgument;
class SILFunctionArgument;
class SILPrintContext;

class SILBasicBlock :
public llvm::ilist_node<SILBasicBlock>, public SILAllocated<SILBasicBlock> {
  friend class SILSuccessor;
  friend class SILFunction;
  friend class SILGlobalVariable;
public:
  using InstListType = llvm::iplist<SILInstruction>;
private:
  /// A backreference to the containing SILFunction.
  SILFunction *Parent;

  /// PrevList - This is a list of all of the terminator operands that are
  /// branching to this block, forming the predecessor list.  This is
  /// automatically managed by the SILSuccessor class.
  SILSuccessor *PredList;

  /// This is the list of basic block arguments for this block.
  std::vector<SILArgument *> ArgumentList;

  /// The ordered set of instructions in the SILBasicBlock.
  InstListType InstList;

  friend struct llvm::ilist_traits<SILBasicBlock>;
  SILBasicBlock() : Parent(nullptr) {}
  void operator=(const SILBasicBlock &) = delete;

  void operator delete(void *Ptr, size_t) SWIFT_DELETE_OPERATOR_DELETED

  SILBasicBlock(SILFunction *F, SILBasicBlock *relativeToBB, bool after);

public:
  ~SILBasicBlock();

  /// Gets the ID (= index in the function's block list) of the block.
  ///
  /// Returns -1 if the block is not contained in a function.
  /// Warning: This function is slow. Therefore it should only be used for
  ///          debug output.
  int getDebugID() const;

  SILFunction *getParent() { return Parent; }
  const SILFunction *getParent() const { return Parent; }

  SILModule &getModule() const;

  /// This method unlinks 'self' from the containing SILFunction and deletes it.
  void eraseFromParent();

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
  iterator erase(SILInstruction *I);

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
    return const_cast<SILBasicBlock *>(this)->getTerminator();
  }

  /// Splits a basic block into two at the specified instruction.
  ///
  /// Note that all the instructions BEFORE the specified iterator
  /// stay as part of the original basic block. The old basic block is left
  /// without a terminator.
  SILBasicBlock *split(iterator I);

  /// Move the basic block to after the specified basic block in the IR.
  ///
  /// Assumes that the basic blocks must reside in the same function. In asserts
  /// builds, an assert verifies that this is true.
  void moveAfter(SILBasicBlock *After);

  /// Moves the instruction to the iterator in this basic block.
  void moveTo(SILBasicBlock::iterator To, SILInstruction *I);

  //===--------------------------------------------------------------------===//
  // SILBasicBlock Argument List Inspection and Manipulation
  //===--------------------------------------------------------------------===//

  using arg_iterator = std::vector<SILArgument *>::iterator;
  using const_arg_iterator = std::vector<SILArgument *>::const_iterator;

  bool args_empty() const { return ArgumentList.empty(); }
  size_t args_size() const { return ArgumentList.size(); }
  arg_iterator args_begin() { return ArgumentList.begin(); }
  arg_iterator args_end() { return ArgumentList.end(); }
  const_arg_iterator args_begin() const { return ArgumentList.begin(); }
  const_arg_iterator args_end() const { return ArgumentList.end(); }

  /// Iterator over the PHI arguments of a basic block.
  /// Defines an implicit cast operator on the iterator, so that this iterator
  /// can be used in the SSAUpdaterImpl.
  template <typename PHIArgT = SILPhiArgument,
            typename IteratorT = arg_iterator>
  class phi_iterator_impl {
  private:
    IteratorT It;

  public:
    explicit phi_iterator_impl(IteratorT A) : It(A) {}
    phi_iterator_impl &operator++() { ++It; return *this; }

    operator PHIArgT *() { return cast<PHIArgT>(*It); }
    bool operator==(const phi_iterator_impl& x) const { return It == x.It; }
    bool operator!=(const phi_iterator_impl& x) const { return !operator==(x); }
  };
  typedef phi_iterator_impl<> phi_iterator;
  typedef phi_iterator_impl<const SILPhiArgument,
                            SILBasicBlock::const_arg_iterator>
      const_phi_iterator;

  inline iterator_range<phi_iterator> phis() {
    return make_range(phi_iterator(args_begin()), phi_iterator(args_end()));
  }
  inline iterator_range<const_phi_iterator> phis() const {
    return make_range(const_phi_iterator(args_begin()),
                      const_phi_iterator(args_end()));
  }

  ArrayRef<SILArgument *> getArguments() const { return ArgumentList; }

  /// Returns a transform array ref that performs llvm::cast<SILPhiArgument> on
  /// each argument and then returns the downcasted value.
  PhiArgumentArrayRef getPhiArguments() const;

  /// Returns a transform array ref that performs
  /// llvm::cast<SILFunctionArgument> on each argument and then returns the
  /// downcasted value.
  FunctionArgumentArrayRef getFunctionArguments() const;

  unsigned getNumArguments() const { return ArgumentList.size(); }
  const SILArgument *getArgument(unsigned i) const { return ArgumentList[i]; }
  SILArgument *getArgument(unsigned i) { return ArgumentList[i]; }

  void cloneArgumentList(SILBasicBlock *Other);

  /// Erase a specific argument from the arg list.
  void eraseArgument(int Index);

  /// Allocate a new argument of type \p Ty and append it to the argument
  /// list. Optionally you can pass in a value decl parameter.
  SILFunctionArgument *createFunctionArgument(SILType Ty,
                                              const ValueDecl *D = nullptr);

  SILFunctionArgument *insertFunctionArgument(unsigned Index, SILType Ty,
                                              ValueOwnershipKind OwnershipKind,
                                              const ValueDecl *D = nullptr) {
    arg_iterator Pos = ArgumentList.begin();
    std::advance(Pos, Index);
    return insertFunctionArgument(Pos, Ty, OwnershipKind, D);
  }

  /// Replace the \p{i}th Function arg with a new Function arg with SILType \p
  /// Ty and ValueDecl \p D.
  SILFunctionArgument *replaceFunctionArgument(unsigned i, SILType Ty,
                                               ValueOwnershipKind Kind,
                                               const ValueDecl *D = nullptr);

  /// Replace the \p{i}th BB arg with a new BBArg with SILType \p Ty and
  /// ValueDecl
  /// \p D.
  SILPhiArgument *replacePhiArgument(unsigned i, SILType Ty,
                                     ValueOwnershipKind Kind,
                                     const ValueDecl *D = nullptr);

  /// Allocate a new argument of type \p Ty and append it to the argument
  /// list. Optionally you can pass in a value decl parameter.
  SILPhiArgument *createPhiArgument(SILType Ty, ValueOwnershipKind Kind,
                                    const ValueDecl *D = nullptr);

  /// Insert a new SILPhiArgument with type \p Ty and \p Decl at position \p
  /// Pos.
  SILPhiArgument *insertPhiArgument(arg_iterator Pos, SILType Ty,
                                    ValueOwnershipKind Kind,
                                    const ValueDecl *D = nullptr);

  SILPhiArgument *insertPhiArgument(unsigned Index, SILType Ty,
                                    ValueOwnershipKind Kind,
                                    const ValueDecl *D = nullptr) {
    arg_iterator Pos = ArgumentList.begin();
    std::advance(Pos, Index);
    return insertPhiArgument(Pos, Ty, Kind, D);
  }

  /// Remove all block arguments.
  void dropAllArguments() { ArgumentList.clear(); }

  //===--------------------------------------------------------------------===//
  // Successors
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

  using const_succ_iterator = TermInst::const_succ_iterator;
  using succ_iterator = TermInst::succ_iterator;

  bool succ_empty() const { return getTerminator()->succ_empty(); }
  succ_iterator succ_begin() { return getTerminator()->succ_begin(); }
  succ_iterator succ_end() { return getTerminator()->succ_end(); }
  const_succ_iterator succ_begin() const {
    return getTerminator()->succ_begin();
  }
  const_succ_iterator succ_end() const { return getTerminator()->succ_end(); }

  using succblock_iterator = TermInst::succblock_iterator;
  using const_succblock_iterator = TermInst::const_succblock_iterator;

  succblock_iterator succblock_begin() {
    return getTerminator()->succblock_begin();
  }
  succblock_iterator succblock_end() {
    return getTerminator()->succblock_end();
  }
  const_succblock_iterator succblock_begin() const {
    return getTerminator()->succblock_begin();
  }
  const_succblock_iterator succblock_end() const {
    return getTerminator()->succblock_end();
  }

  SILBasicBlock *getSingleSuccessorBlock() {
    return getTerminator()->getSingleSuccessorBlock();
  }

  const SILBasicBlock *getSingleSuccessorBlock() const {
    return getTerminator()->getSingleSuccessorBlock();
  }

  /// Returns true if \p BB is a successor of this block.
  bool isSuccessorBlock(SILBasicBlock *Block) const {
    return getTerminator()->isSuccessorBlock(Block);
  }

  using SuccessorBlockListTy = TermInst::SuccessorBlockListTy;
  using ConstSuccessorBlockListTy = TermInst::ConstSuccessorBlockListTy;

  /// Return the range of SILBasicBlocks that are successors of this block.
  SuccessorBlockListTy getSuccessorBlocks() {
    return getTerminator()->getSuccessorBlocks();
  }

  /// Return the range of SILBasicBlocks that are successors of this block.
  ConstSuccessorBlockListTy getSuccessorBlocks() const {
    return getTerminator()->getSuccessorBlocks();
  }

  //===--------------------------------------------------------------------===//
  // Predecessors
  //===--------------------------------------------------------------------===//

  using pred_iterator = SILSuccessor::pred_iterator;

  bool pred_empty() const { return PredList == nullptr; }
  pred_iterator pred_begin() const { return pred_iterator(PredList); }
  pred_iterator pred_end() const { return pred_iterator(); }

  iterator_range<pred_iterator> getPredecessorBlocks() const {
    return {pred_begin(), pred_end()};
  }

  bool isPredecessorBlock(SILBasicBlock *BB) const {
    return any_of(
        getPredecessorBlocks(),
        [&BB](const SILBasicBlock *PredBB) -> bool { return BB == PredBB; });
  }

  SILBasicBlock *getSinglePredecessorBlock() {
    if (pred_empty() || std::next(pred_begin()) != pred_end())
      return nullptr;
    return *pred_begin();
  }

  const SILBasicBlock *getSinglePredecessorBlock() const {
    return const_cast<SILBasicBlock *>(this)->getSinglePredecessorBlock();
  }

  //===--------------------------------------------------------------------===//
  // Utility
  //===--------------------------------------------------------------------===//

  /// Returns true if this BB is the entry BB of its parent.
  bool isEntry() const;

  /// Returns true if this block ends in an unreachable or an apply of a
  /// no-return apply or builtin.
  bool isNoReturn() const;

  /// Returns true if this block only contains a branch instruction.
  bool isTrampoline() const;

  /// Returns true if it is legal to hoist instructions into this block.
  ///
  /// Used by llvm::LoopInfo.
  bool isLegalToHoistInto() const;

  /// Returns the debug scope of the first non-meta instructions in the
  /// basic block. SILBuilderWithScope uses this to correctly set up
  /// the debug scope for newly created instructions.
  const SILDebugScope *getScopeOfFirstNonMetaInstruction();

  //===--------------------------------------------------------------------===//
  // Debugging
  //===--------------------------------------------------------------------===//

  /// Pretty-print the SILBasicBlock.
  void dump() const;

  /// Pretty-print the SILBasicBlock with the designated stream.
  void print(llvm::raw_ostream &OS) const;

  /// Pretty-print the SILBasicBlock with the designated stream and context.
  void print(llvm::raw_ostream &OS, SILPrintContext &Ctx) const;

  void printAsOperand(raw_ostream &OS, bool PrintType = true);

  /// getSublistAccess() - returns pointer to member of instruction list
  static InstListType SILBasicBlock::*getSublistAccess() {
    return &SILBasicBlock::InstList;
  }

  /// Drops all uses that belong to this basic block.
  void dropAllReferences() {
    dropAllArguments();
    for (SILInstruction &I : *this)
      I.dropAllReferences();
  }

  void eraseInstructions();

private:
  friend class SILArgument;

  /// BBArgument's ctor adds it to the argument list of this block.
  void insertArgument(arg_iterator Iter, SILArgument *Arg) {
    ArgumentList.insert(Iter, Arg);
  }

  /// Insert a new SILFunctionArgument with type \p Ty and \p Decl at position
  /// \p Pos.
  SILFunctionArgument *insertFunctionArgument(arg_iterator Pos, SILType Ty,
                                              ValueOwnershipKind OwnershipKind,
                                              const ValueDecl *D = nullptr);
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
struct ilist_traits<::swift::SILBasicBlock>
  : ilist_node_traits<::swift::SILBasicBlock> {
  using SelfTy = ilist_traits<::swift::SILBasicBlock>;
  using SILBasicBlock = ::swift::SILBasicBlock;
  using SILFunction = ::swift::SILFunction;
  using FunctionPtrTy = ::swift::NullablePtr<SILFunction>;

private:
  friend class ::swift::SILFunction;

  SILFunction *Parent;
  using block_iterator = simple_ilist<SILBasicBlock>::iterator;

public:
  static void deleteNode(SILBasicBlock *BB) { BB->~SILBasicBlock(); }

  void transferNodesFromList(ilist_traits<SILBasicBlock> &SrcTraits,
                             block_iterator First, block_iterator Last);
private:
  static void createNode(const SILBasicBlock &);
};

} // end llvm namespace

#endif
