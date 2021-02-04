//===--- CFG.h - SIL control-flow graph utilities ---------------*- C++ -*-===//
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
// This file provides basic declarations and utilities for working with
// SIL basic blocks as a control-flow graph.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CFG_H
#define SWIFT_SIL_CFG_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/GraphTraits.h"

#if defined(__has_include)
#if __has_include("llvm/Support/CfgTraits.h")
#include "llvm/Support/CfgTraits.h"
#define SWIFT_LLVM_HAS_CFGTRAITS_H
#endif
#endif

namespace llvm {

//===----------------------------------------------------------------------===//
// GraphTraits for SILBasicBlock
//===----------------------------------------------------------------------===//

template <> struct GraphTraits<swift::SILBasicBlock *> {
  using ChildIteratorType = swift::SILBasicBlock::succblock_iterator;
  using Node = swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->succblock_begin();
  }
  static ChildIteratorType child_end(NodeRef N) { return N->succblock_end(); }
};

template <> struct GraphTraits<const swift::SILBasicBlock*> {
  using ChildIteratorType = swift::SILBasicBlock::const_succblock_iterator;
  using Node = const swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->succblock_begin();
  }
  static ChildIteratorType child_end(NodeRef N) { return N->succblock_end(); }
};

template <> struct GraphTraits<Inverse<swift::SILBasicBlock*> > {
  using ChildIteratorType = swift::SILBasicBlock::pred_iterator;
  using Node = swift::SILBasicBlock;
  using NodeRef = Node *;
  static NodeRef getEntryNode(Inverse<swift::SILBasicBlock *> G) {
    return G.Graph;
  }
  static inline ChildIteratorType child_begin(NodeRef N) {
    return N->pred_begin();
  }
  static inline ChildIteratorType child_end(NodeRef N) {
    return N->pred_end();
  }
};

template <> struct GraphTraits<Inverse<const swift::SILBasicBlock*> > {
  using ChildIteratorType = swift::SILBasicBlock::pred_iterator;
  using Node = const swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(Inverse<const swift::SILBasicBlock *> G) {
    return G.Graph;
  }
  static inline ChildIteratorType child_begin(NodeRef N) {
    return N->pred_begin();
  }
  static inline ChildIteratorType child_end(NodeRef N) {
    return N->pred_end();
  }
};

template <>
struct GraphTraits<swift::SILFunction *>
    : public GraphTraits<swift::SILBasicBlock *> {
  using GraphType = swift::SILFunction *;
  using NodeRef = swift::SILBasicBlock *;

  static NodeRef getEntryNode(GraphType F) { return &F->front(); }

  using nodes_iterator = pointer_iterator<swift::SILFunction::iterator>;
  static nodes_iterator nodes_begin(GraphType F) {
    return nodes_iterator(F->begin());
  }
  static nodes_iterator nodes_end(GraphType F) {
    return nodes_iterator(F->end());
  }
  static unsigned size(GraphType F) { return F->size(); }
};

template <> struct GraphTraits<Inverse<swift::SILFunction*> >
    : public GraphTraits<Inverse<swift::SILBasicBlock*> > {
  using GraphType = Inverse<swift::SILFunction *>;
  using NodeRef = NodeRef;

  static NodeRef getEntryNode(GraphType F) { return &F.Graph->front(); }

  using nodes_iterator = pointer_iterator<swift::SILFunction::iterator>;
  static nodes_iterator nodes_begin(GraphType F) {
    return nodes_iterator(F.Graph->begin());
  }
  static nodes_iterator nodes_end(GraphType F) {
    return nodes_iterator(F.Graph->end());
  }
  static unsigned size(GraphType F) { return F.Graph->size(); }
};

#ifdef SWIFT_LLVM_HAS_CFGTRAITS_H

class SILCfgTraitsBase : public CfgTraitsBase {
public:
  using ParentType = swift::SILFunction;
  using BlockRef = swift::SILBasicBlock *;
  using ValueRef = swift::SILInstruction *;

  static CfgBlockRef wrapRef(BlockRef block) {
    return makeOpaque<CfgBlockRefTag>(block);
  }
  static CfgValueRef wrapRef(ValueRef block) {
    return makeOpaque<CfgValueRefTag>(block);
  }
  static BlockRef unwrapRef(CfgBlockRef block) {
    return static_cast<BlockRef>(getOpaque(block));
  }
  static ValueRef unwrapRef(CfgValueRef block) {
    return static_cast<ValueRef>(getOpaque(block));
  }
};
/// \brief CFG traits for SIL IR.
class SILCfgTraits : public CfgTraits<SILCfgTraitsBase, SILCfgTraits> {
public:
  explicit SILCfgTraits(swift::SILFunction * /*parent*/) {}

  static swift::SILFunction *getBlockParent(swift::SILBasicBlock *block) {
    return block->getParent();
  }

  static auto predecessors(swift::SILBasicBlock *block) {
    return block->getPredecessorBlocks();
  }
  static auto successors(swift::SILBasicBlock *block) {
    return block->getSuccessors();
  }

  /// Get the defining block of a value if it is an instruction, or null
  /// otherwise.
  static BlockRef getValueDefBlock(ValueRef value) {
    if (auto *instruction = dyn_cast<swift::SILInstruction>(value))
      return instruction->getParent();
    return nullptr;
  }

  struct block_iterator
      : iterator_adaptor_base<block_iterator, swift::SILFunction::iterator> {
    using Base = iterator_adaptor_base<block_iterator, swift::SILFunction::iterator>;

    block_iterator() = default;

    explicit block_iterator(swift::SILFunction::iterator i) : Base(i) {}

    swift::SILBasicBlock *operator*() const { return &Base::operator*(); }
  };

  static iterator_range<block_iterator> blocks(swift::SILFunction *function) {
    return {block_iterator(function->begin()), block_iterator(function->end())};
  }

  struct value_iterator
      : iterator_adaptor_base<value_iterator, swift::SILBasicBlock::iterator> {
    using Base = iterator_adaptor_base<value_iterator, swift::SILBasicBlock::iterator>;

    value_iterator() = default;

    explicit value_iterator(swift::SILBasicBlock::iterator i) : Base(i) {}

    ValueRef operator*() const { return &Base::operator*(); }
  };

  static iterator_range<value_iterator> blockdefs(BlockRef block) {
    return {value_iterator(block->begin()), value_iterator(block->end())};
  }
  struct Printer {
    explicit Printer(const SILCfgTraits &) {}
    ~Printer(){}

    void printBlockName(raw_ostream &out, BlockRef block) const {
      block->printAsOperand(out);
    }
    void printValue(raw_ostream &out, ValueRef value) const {
      value->print(out);
    }
  };
};

template <> struct CfgTraitsFor<swift::SILBasicBlock> {
  using CfgTraits = SILCfgTraits;
};

#endif

} // end llvm namespace

#endif

